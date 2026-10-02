(* Token minting for boot-time identities (root, fed peers, sabralib).
   Shared so the seeding module never imports api.ml. *)

let random_token_hex () =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let raw = really_input_string ic 32 in
      let hex = Buffer.create 64 in
      String.iter
        (fun c -> Buffer.add_string hex (Printf.sprintf "%02x" (Char.code c)))
        raw;
      Buffer.contents hex)
