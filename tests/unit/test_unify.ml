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

let test_apply_subst_deep_term () =
  let rec deep n acc =
    if n = 0 then acc else deep (n - 1) (Fun ("f", [ acc ]))
  in
  let t = deep 20000 (Var "X") in
  let s = Types.StringMap.singleton "X" (Fun ("a", [])) in
  match Subst.apply_subst_term s t with
  | Fun _ -> ()
  | Var _ -> fail "deep substitution should rebuild the term"

let () =
  run "unification"
    [
      ("basic",
       [
         test_case "same atom" `Quick test_unify_same_atom;
         test_case "var with const" `Quick test_unify_var_const;
         test_case "deep substitution" `Quick test_apply_subst_deep_term;
       ]);
    ]
