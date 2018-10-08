open Ctypes
open Foreign
open Decompress

let inflate inbuf insize outbuf outsize _level _log _window =
  let inbuf = bigarray_of_ptr array1 insize Bigarray.char inbuf in
  let outbuf = bigarray_of_ptr array1 outsize Bigarray.char outbuf in
  let window = Window.create ~crc:Window.adler32 ~witness:B.bigstring in
  let open Zlib_inflate in
  let state = default ~witness:B.bigstring window in
  let inpos = ref 0 in
  let outpos = ref 0 in
  let rec go state =
    match eval inbuf outbuf state with
    | `Await state ->
        inpos := !inpos + used_in state ;
        go (refill !inpos (insize - !inpos) state)
    | `Flush state ->
        outpos := !outpos + used_out state ;
        go (flush !outpos (outsize - !outpos) state)
    | `End state ->
        outpos := !outpos + used_out state ;
        Ok !outpos
    | `Error (_, err) -> Error err
  in
  go state
  |> function Ok v -> v | Error _ -> invalid_arg "Decompress.inflate"

let deflate inbuf insize outbuf outsize level _log _window =
  let inbuf = bigarray_of_ptr array1 insize Bigarray.char inbuf in
  let outbuf = bigarray_of_ptr array1 outsize Bigarray.char outbuf in
  let open Zlib_deflate in
  let state = default ~witness:B.bigstring level in
  let inpos = ref 0 in
  let outpos = ref 0 in
  let rec go state =
    match eval inbuf outbuf state with
    | `Await state ->
        inpos := !inpos + used_in state ;
        if !inpos >= insize then go (finish state)
        else go (no_flush !inpos (insize - !inpos) state)
    | `Flush state ->
        outpos := !outpos + used_out state ;
        if !outpos >= outsize then invalid_arg "Not enough spaces" ;
        go (flush !outpos (outsize - !outpos) state)
    | `End state ->
        outpos := !outpos + used_out state ;
        Ok !outpos
    | `Error (_, err) -> Error err
  in
  go state
  |> function Ok v -> v | Error _ -> invalid_arg "Decompress.deflate"

module Stubs (I : Cstubs_inverted.INTERNAL) = struct
  let () =
    I.internal "dcpr_inflate"
      ( ptr char @-> int @-> ptr char @-> int @-> int @-> int @-> ptr void
      @-> returning int )
      inflate

  let () =
    I.internal "dcpr_deflate"
      ( ptr char @-> int @-> ptr char @-> int @-> int @-> int @-> ptr void
      @-> returning int )
      deflate
end
