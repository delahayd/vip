open Alcotest
open Prover_lib
open Types

let var x = Var x
let const c = Fun (c, [])
let atom p args = { pred = p; args = args }
let pos a = Pos a
let neg a = Neg a

let test_fvi_add_and_retrieve () =
  let fv = Feature_vector.create () in
  (* Clause 1: P(a) *)
  let c1 = [ pos (atom "p" [const "a"]) ] in
  Feature_vector.add fv c1 1;
  
  (* Clause 2: P(X) *)
  let c2 = [ pos (atom "p" [var "X"]) ] in
  Feature_vector.add fv c2 2;

  (* P(X) should be a candidate to subsume P(a) *)
  let subsuming = Feature_vector.find_subsuming_candidates fv c1 in
  check bool "C2 is a subsuming candidate for C1" true (List.mem 2 subsuming);

  (* P(a) should be a candidate to be subsumed by P(X) *)
  let subsumed = Feature_vector.find_subsumed_candidates fv c2 in
  check bool "C1 is a subsumed candidate for C2" true (List.mem 1 subsumed)

let test_fvi_filtering () =
  let fv = Feature_vector.create () in
  (* Clause 1: P(a) *)
  let c1 = [ pos (atom "p" [const "a"]) ] in
  Feature_vector.add fv c1 1;
  
  (* Clause 2: Q(b) *)
  let c2 = [ pos (atom "q" [const "b"]) ] in
  Feature_vector.add fv c2 2;

  (* P(a) should NOT be a candidate to subsume Q(b) *)
  let subsuming = Feature_vector.find_subsuming_candidates fv c2 in
  check bool "C1 is NOT a subsuming candidate for C2" false (List.mem 1 subsuming)

let test_fvi_remove () =
  let fv = Feature_vector.create () in
  let c1 = [ pos (atom "p" [const "a"]) ] in
  Feature_vector.add fv c1 1;
  Feature_vector.add fv c1 2; (* Identical clause, different ID *)

  Feature_vector.remove fv 1;

  (* ID 1 should be gone, but ID 2 should remain *)
  let candidates = Feature_vector.find_subsumed_candidates fv c1 in
  check bool "ID 1 is removed" false (List.mem 1 candidates);
  check bool "ID 2 is still present" true (List.mem 2 candidates)

let test_fvi_complex_clause () =
  let fv = Feature_vector.create () in
  (* C1: P(X) v Q(Y) *)
  let c1 = [ pos (atom "p" [var "X"]); pos (atom "q" [var "Y"]) ] in
  Feature_vector.add fv c1 1;
  
  (* C2: P(a) v Q(b) v R(c) *)
  let c2 = [ pos (atom "p" [const "a"]); pos (atom "q" [const "b"]); pos (atom "r" [const "c"]) ] in
  Feature_vector.add fv c2 2;

  (* C1 should be a candidate to subsume C2 *)
  let subsuming = Feature_vector.find_subsuming_candidates fv c2 in
  check bool "C1 subsumes C2 (candidate)" true (List.mem 1 subsuming);

  (* C2 should NOT be a candidate to subsume C1 (it has more literals/features) *)
  let subsuming_c1 = Feature_vector.find_subsuming_candidates fv c1 in
  check bool "C2 does NOT subsume C1 (candidate)" false (List.mem 2 subsuming_c1)


let () =
  run "Feature Vector Index"
    [
      ("basic_operations",
       [
         test_case "add and retrieve candidates" `Quick test_fvi_add_and_retrieve;
         test_case "filter disjoint clauses" `Quick test_fvi_filtering;
         test_case "remove specific IDs" `Quick test_fvi_remove;
         test_case "complex clause subset/superset filtering" `Quick test_fvi_complex_clause;
       ]);
    ]
