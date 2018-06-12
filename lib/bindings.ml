open Ctypes
open Foreign
open Decompress

let inflate inbuf insize outbuf outsize _level _log _window =
  let open Zlib_inflate in

  let inbuf = Bytes.unsafe_of_string (string_from_ptr inbuf ~length:insize) in
  let outbuf = Bytes.unsafe_of_string (string_from_ptr outbuf ~length:outsize) in

  let state  = default (Window.create ~proof:B.proof_bytes) in
  let inpos  = ref 0 in
  let outpos = ref 0 in
  let inbuf  = B.from_bytes inbuf in
  let outbuf = B.from_bytes outbuf in

  let rec go state = match eval inbuf outbuf state with
    | `Await state ->
       inpos := !inpos + (used_in state);
       go (refill !inpos (insize - !inpos) state)
    | `Flush state ->
       outpos := !outpos + (used_out state);
       go (flush !outpos (outsize - !outpos) state)
    | `End _ ->
       outpos := !outpos + (used_out state);
       Ok !outpos
    | `Error (_, err) -> Error err in
  go state |> function
             | Ok v -> v
             | Error _ -> invalid_arg "Decompress.inflate"

let deflate inbuf insize outbuf outsize level _log _window =
  let inbuf = bigarray_of_ptr array1 insize Bigarray.char inbuf in
  let outbuf = bigarray_of_ptr array1 outsize Bigarray.char outbuf in

  let open Zlib_deflate in

  let state  = default ~proof:B.proof_bigstring level in
  let inpos  = ref 0 in
  let outpos = ref 0 in
  let inbuf  = B.from_bigstring inbuf in
  let outbuf = B.from_bigstring outbuf in

  let rec go state =
    match eval inbuf outbuf state with
    | `Await state ->
       inpos := !inpos + used_in state;

       if !inpos >= insize
       then go (finish state)
       else go (no_flush !inpos (insize - !inpos) state)
    | `Flush state ->
       outpos := !outpos + (used_out state);

       if !outpos >= outsize
       then invalid_arg "Not enough spaces";

       go (flush !outpos (outsize - !outpos) state)
    | `End state ->
       outpos := !outpos + (used_out state);
       Ok !outpos
    | `Error (_, err) -> Error err in
  go state |> function
             | Ok v -> v
             | Error _ -> invalid_arg "Decompress.deflate"

module Stubs (I: Cstubs_inverted.INTERNAL) =
  struct
    let () = I.internal "inflate" (ptr char @-> int @-> ptr char @-> int @-> int @-> bool @-> ptr void @-> returning int) inflate
    let () = I.internal "deflate" (ptr char @-> int @-> ptr char @-> int @-> int @-> bool @-> ptr void @-> returning int) deflate
  end
