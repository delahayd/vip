open Alcotest
open Prover_lib
open Types
open Resolution

let var x = Var x
let const c = Fun (c, [])
let fun_ f args = Fun (f, args)

let atom p args = { pred = p; args = args }
let pos a = Pos a
let neg a = Neg a

let eq l r = pos (atom "=" [l; r])
let neq l r = neg (atom "=" [l; r])

let c1 = [ pos (atom "p" [var "X"]); pos (atom "q" [var "X"]) ]
let c2 = [ neg (atom "p" [const "a"]); pos (atom "r" [var "Y"]) ]

let test_binary_resolution_unrestricted () =
  let res = Resolution.test_resolve Unrestricted c1 c2 in
  (* Expected: Q(a) v R(V0) because rename_clause_apart renames Y to V0 *)
  let expected = [ pos (atom "q" [const "a"]); pos (atom "r" [var "V0"]) ] in
  match res with
  | [ r ] ->
      check string "resolved clause" (Pretty.string_of_clause expected) (Pretty.string_of_clause r)
  | _ -> fail "Should generate exactly one resolvent"

let c3 = [ pos (atom "p" [var "X"]); pos (atom "p" [const "a"]) ]

let test_factoring_unrestricted () =
  let res = Resolution.test_factor Unrestricted c3 in
  (* Expected: P(a) *)
  let expected = [ pos (atom "p" [const "a"]) ] in
  match res with
  | [ r ] ->
      check string "factored clause" (Pretty.string_of_clause expected) (Pretty.string_of_clause r)
  | _ -> fail "Should generate exactly one factor"

let test_equality_resolution () =
  (* X != a v P(X) -> P(a) *)
  let c = [ neq (var "X") (const "a"); pos (atom "p" [var "X"]) ] in
  let res = Resolution.equality_resolution ~check_timeout:(fun () -> ()) Unrestricted c in
  let expected = [ pos (atom "p" [const "a"]) ] in
  match res with
  | [ r ] ->
      check string "equality resolved" (Pretty.string_of_clause expected) (Pretty.string_of_clause r)
  | _ -> fail "Should generate exactly one equality resolvent"

let test_equality_factoring () =
  (* X = a v X = b -> a = b v X = b (Wait, paramodulation is complex. We test simple equality factoring if implemented) *)
  let c = [ eq (var "X") (const "a"); eq (var "X") (const "b") ] in
  let res = Resolution.equality_factoring ~check_timeout:(fun () -> ()) Unrestricted c in
  (* Engine produces: =(V0,b) | ~=(a,b) *)
  let expected = [ eq (var "V0") (const "b"); neq (const "a") (const "b") ] in
  match res with
  | [ r ] ->
      (* Order might differ, checking exact string for now. Expected output structure depends on implementation. *)
      check string "equality factored" (Pretty.string_of_clause expected) (Pretty.string_of_clause r)
  | _ -> fail "Should generate exactly one equality factor"

let () =
  run "Resolution and Factoring"
    [
      ("resolution",
       [
         test_case "binary resolution unrestricted" `Quick test_binary_resolution_unrestricted;
         test_case "equality resolution" `Quick test_equality_resolution;
       ]);
      ("factoring",
       [
         test_case "factoring unrestricted" `Quick test_factoring_unrestricted;
         test_case "equality factoring" `Quick test_equality_factoring;
       ])
    ]
