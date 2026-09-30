(* throwaway: what does the engine actually pass as prim args? *)
module Eng = Tuna_interp.Prim_eval.Make (struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let catch f h = try f () with e -> h e
end)

let () =
  let art = Tuna_compiler.Bracket.compile_source "(lambda (x) (prim \"echo\" x))" in
  Printf.printf "program ternary: %s\n" art.Tuna_compiler.Bracket.ternary;
  let calls = ref [] in
  let host ~site ~name ~args =
    calls := (site, name, Tuna.Canon.encode args) :: !calls;
    `Ok args
  in
  let outcome =
    Eng.eval ~host ~mode:Eng.Canonical ~fuel:10000 ~size_cap:100000
      ~deadline:Float.infinity ~program:art.Tuna_compiler.Bracket.tree
      [ Tuna.Canon.parse "10" ]
  in
  (match outcome with
   | Eng.Normal (t, s) ->
       Printf.printf "outcome: normal %s steps=%d\n" (Tuna.Canon.encode t) s
   | Eng.Loop s -> Printf.printf "loop %d\n" s
   | Eng.Fuel_exhausted s -> Printf.printf "fuel %d\n" s
   | Eng.Size_exhausted s -> Printf.printf "size %d\n" s
   | Eng.Deadline_exceeded s -> Printf.printf "deadline %d\n" s);
  List.iter
    (fun (s, n, a) -> Printf.printf "call site=%d name=%s args=%s\n" s n a)
    (List.rev !calls)
