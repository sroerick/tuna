(* Direct: the identity monad.

   rim-eio (borg/rim-eio.borg §store): the store's Lwt coloring is
   removed in place.  [t = 'a], [return = identity], [>>= = apply] —
   so a former [>>=]-chain is a sequence of ordinary computations on
   the calling fiber, and there is no promise, scheduler, or bridge in
   the call path (family law: direct-style everywhere, no Lwt_eio).

   This module exists only so the mechanical re-threading of store.ml
   (and the other non-HTTP server modules) can stand: the semantics are
   exactly the "mechanical lets where >>= chains stood" the chapter
   names.  [catch]/[finalize] keep exception semantics identical to the
   Lwt originals. *)

type 'a t = 'a

let return x = x
let return_unit = ()
let return_none = None
let return_some x = Some x
let fail e = raise e
let bind x f = f x
let ( >>= ) = bind
let catch f h = try f () with e -> h e
let finalize f ~finally = Fun.protect ~finally:(fun () -> finally ()) f

module Infix = struct
  let ( >>= ) = bind
end

module List = struct
  include Stdlib.List

  let iter_s f xs = List.iter f xs
  let map_s f xs = List.map f xs
end
