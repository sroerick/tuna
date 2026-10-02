(* Tuna_store.Credentials: the interim password-hash format + randomness.

   INTERIM, honestly recorded (pp-slice, 0013): the v1 format is
   PP-compatible legacy

       sha256$<salt_hex>$<digest_hex>    digest = sha256(salt_hex ^ ":" ^ password)

   exactly the interim scheme pricklypear shipped before upgrading to
   argon2id with transparent rehash-on-success.  We pin the same string
   shape and digest recipe so that a future argon2id champion can land
   the same way: verify legacy rows, rewrite on success — no flag day.

   A keyed hash (HMAC) with a server-secret pepper would resist offline
   dictionary attacks against a stolen DB dump.  The pepper is not
   implemented in v1 (the slice has no trust anchor yet); the README
   and the chapter record this as a known limitation of the interim
   scheme, not as a hidden choice. *)

open Digestif.SHA256

let hex_encode (b : string) : string =
  let len = String.length b in
  let buf = Bytes.create (len * 2) in
  let hex = "0123456789abcdef" in
  for i = 0 to len - 1 do
    let c = Char.code b.[i] in
    Bytes.set buf (i * 2) hex.[c lsr 4];
    Bytes.set buf (i * 2 + 1) hex.[c land 0xf]
  done;
  Bytes.unsafe_to_string buf

let sha256_hex (s : string) : string = digest_string s |> to_hex

(* n random bytes as lowercase hex (/dev/urandom, same source as the
   boot bearer tokens: no crypto library in the switch to lean on). *)
let random_hex (n : int) : string =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> hex_encode (really_input_string ic n))

(* constant-time compare: no early exit on first differing byte, and
   length mismatch folds into the accumulator so an attacker cannot
   even learn the digest's length from response time *)
let secure_equal (a : string) (b : string) : bool =
  let la = String.length a and lb = String.length b in
  let len = max la lb in
  let acc = ref (la lxor lb) in
  for i = 0 to len - 1 do
    let ca = if i < la then Char.code a.[i] else 0 in
    let cb = if i < lb then Char.code b.[i] else 0 in
    acc := !acc lor (ca lxor cb)
  done;
  !acc = 0

(* Hash a password into the interim format.  Salt is 16 bytes (32 hex
   chars), a fresh one per row; the digest binds the salt so verify
   never consults anything but the stored string. *)
let hash_password (password : string) : string =
  let salt_hex = random_hex 16 in
  let digest = sha256_hex (salt_hex ^ ":" ^ password) in
  Printf.sprintf "sha256$%s$%s" salt_hex digest

(* Verify against a stored interim-format hash.  Malformed stored
   strings (later formats, corruption) simply fail: a future argon2id
   champion dispatches on a "$argon2id$" prefix and never lands here. *)
let verify_password (password : string) ~(stored : string) : bool =
  match String.split_on_char '$' stored with
  | [ "sha256"; salt_hex; digest ] when String.length salt_hex > 0
                                      && String.length digest > 0 ->
      secure_equal (sha256_hex (salt_hex ^ ":" ^ password)) digest
  | _ -> false

(* A fixed dummy stem so unknown-username logins pay the same hash work
   as known ones (timing flatten, PP auth_store pattern): never compared
   for success, only spent.  Its digest is over a sentinel password no
   caller can supply, so a verify against it can never return true. *)
let dummy_hash =
  "sha256$00000000000000000000000000000000$"
  ^ sha256_hex "00000000000000000000000000000000:no-such-user-dummy"

(* Fresh opaque session token: 32 bytes, 64 hex chars — same entropy
   class as the boot bearer tokens (server's Tokens.random_token_hex),
   looked up only as sha256 in auth_sessions. *)
let mint_session_token () : string = random_hex 32
