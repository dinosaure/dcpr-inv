open Ctypes
open Foreign
open Decompress

let inflate inbuf insize outbuf outsize _level _log _window =
  let open Zlib_inflate in

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
  Format.printf "> start call deflate.\n%!";

  let open Zlib_deflate in

  let state  = default ~proof:B.proof_bytes level in
  let inpos  = ref 0 in
  let outpos = ref 0 in
  let inbuf  = B.from_bytes inbuf in
  let outbuf = B.from_bytes outbuf in

  Format.printf "> start deflate.\n%!";

  let rec go state = match eval inbuf outbuf state with
    | `Await state ->
       inpos := !inpos + used_in state;

       if !inpos = insize
       then go (finish state)
       else go (no_flush !inpos (insize - !inpos) state)
    | `Flush state ->
       outpos := !outpos + (used_out state);
       go (flush !outpos (outsize - !outpos) state)
    | `End _ ->
       outpos := !outpos + (used_out state);
       Ok !outpos
    | `Error (_, err) -> Error err in
  go state |> function
             | Ok v -> v
             | Error _ -> invalid_arg "Decompress.deflate"

module Stubs (I: Cstubs_inverted.INTERNAL) =
  struct
    let () = I.internal "inflate" (string @-> int @-> string @-> int @-> int @-> bool @-> ptr void @-> returning int) inflate
    let () = I.internal "deflate" (string @-> int @-> string @-> int @-> int @-> bool @-> ptr void @-> returning int) deflate
  end
