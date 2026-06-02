open Alcotest
open Prover_lib
open Types

let test_unify_same_atom () =
  let a1 = { pred = "p"; args = [ Fun ("a", []) ] } in
  let a2 = { pred = "p"; args = [ Fun ("a", []) ] } in
  let _ = Unif.unify_atoms a1 a2 Subst.empty_subst in
  ()

let test_unify_var_const () =
  let a1 = { pred = "p"; args = [ Var "X" ] } in
  let a2 = { pred = "p"; args = [ Fun ("a", []) ] } in
  let s = Unif.unify_atoms a1 a2 Subst.empty_subst in
  let t = Subst.apply_subst_term s (Var "X") in
  check string "X -> a" "a" (Pretty.string_of_term t)

let () =
  run "unification"
    [
      ("basic",
       [
         test_case "same atom" `Quick test_unify_same_atom;
         test_case "var with const" `Quick test_unify_var_const;
       ]);
    ]
