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
  let res = Resolution.equality_resolution ~check_timeout:(fun () -> ()) ~emulate_v1:false Unrestricted c in
  let expected = [ pos (atom "p" [const "a"]) ] in
  match res with
  | [ r ] ->
      check string "equality resolved" (Pretty.string_of_clause expected) (Pretty.string_of_clause r)
  | _ -> fail "Should generate exactly one equality resolvent"

let test_equality_factoring () =
  (* X = a v X = b -> a = b v X = b (Wait, paramodulation is complex. We test simple equality factoring if implemented) *)
  let c = [ eq (var "X") (const "a"); eq (var "X") (const "b") ] in
  let res = Resolution.equality_factoring ~check_timeout:(fun () -> ()) ~emulate_v1:false Unrestricted c in
  (* Engine produces: =(V0,b) | ~=(a,b) *)
  let expected = [ eq (var "V0") (const "b"); neq (const "a") (const "b") ] in
  match res with
  | [ r ] ->
      (* Order might differ, checking exact string for now. Expected output structure depends on implementation. *)
      check string "equality factored" (Pretty.string_of_clause expected) (Pretty.string_of_clause r)
  | _ -> fail "Should generate exactly one equality factor"

let test_polarized_resolution_uses_selected_one_way_literal () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let theory = [ [ neg (atom "p" []); pos (atom "q" []) ] ] in
  let support = [ [ pos (atom "p" []) ]; [ neg (atom "q" []) ] ] in
  let res =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 100 }
      ~expensive_simplifications:false
      ~one_way_clauses:theory
      ~mode:Polarized
      ~axioms:theory
      ~support
      ()
  in
  match res.stop_reason with
  | Refutation_found _ -> ()
  | _ -> fail "Polarized mode should refute through the selected one-way literal"

let test_polarized_resolution_blocks_one_way_and_non_selected_literals () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let theory =
    [
      [ neg (atom "p" []); pos (atom "q" []); pos (atom "r" []) ];
      [ pos (atom "p" []) ];
    ]
  in
  let support = [ [ neg (atom "q" []) ] ] in
  let res =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 100 }
      ~expensive_simplifications:false
      ~one_way_clauses:theory
      ~mode:Polarized
      ~axioms:theory
      ~support
      ()
  in
  match res.stop_reason with
  | Saturation -> ()
  | Refutation_found _ ->
      fail "Polarized mode must not resolve two one-way clauses or use a non-selected one-way literal"
  | Time_limit | Clause_limit -> fail "Polarized restriction test should saturate quickly"

let test_polarized_atom_rewrite_normalizes_one_way_definition () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let definition = [ neg (atom "p" [ var "X" ]); pos (atom "q" [ var "X" ]) ] in
  let support =
    [
      [ pos (atom "p" [ const "a" ]) ];
      [ neg (atom "q" [ const "a" ]) ];
    ]
  in
  let res =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 100 }
      ~expensive_simplifications:false
      ~one_way_clauses:[ definition ]
      ~mode:Polarized
      ~axioms:[ definition ]
      ~support
      ()
  in
  match res.stop_reason with
  | Refutation_found _ ->
      check bool "atom rewrite used" true (res.stats.demodulation_rewrites > 0)
  | _ -> fail "Polarized mode should normalize p(a) to q(a)"

let test_polarized_atom_rewrite_does_not_rewrite_negative_literals () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let definition = [ neg (atom "p" [ var "X" ]); pos (atom "q" [ var "X" ]) ] in
  let support =
    [
      [ neg (atom "p" [ const "a" ]) ];
      [ pos (atom "q" [ const "a" ]) ];
    ]
  in
  let res =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 100 }
      ~expensive_simplifications:false
      ~one_way_clauses:[ definition ]
      ~mode:Polarized
      ~axioms:[ definition ]
      ~support
      ()
  in
  match res.stop_reason with
  | Saturation -> ()
  | Refutation_found _ ->
      fail "P => Q must not rewrite ~P into ~Q"
  | Time_limit | Clause_limit -> fail "negative rewrite soundness test should saturate quickly"

let test_polarized_resolution_without_support_uses_ordinary_axioms () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let theory = [ [ pos (atom "p" []) ]; [ neg (atom "p" []) ] ] in
  let res =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 100 }
      ~expensive_simplifications:false
      ~mode:Polarized
      ~axioms:theory
      ~support:[]
      ()
  in
  match res.stop_reason with
  | Refutation_found _ -> ()
  | _ -> fail "Polarized mode should fall back to ordinary axioms without support"

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
       ]);
      ("polarized",
       [
         test_case
           "selected one-way literal"
           `Quick
           test_polarized_resolution_uses_selected_one_way_literal;
         test_case
           "blocks one-way/non-selected"
           `Quick
           test_polarized_resolution_blocks_one_way_and_non_selected_literals;
         test_case
           "atom rewrite normalizes one-way definition"
           `Quick
           test_polarized_atom_rewrite_normalizes_one_way_definition;
         test_case
           "atom rewrite does not rewrite negative literals"
           `Quick
           test_polarized_atom_rewrite_does_not_rewrite_negative_literals;
         test_case
           "ordinary axioms without support"
           `Quick
           test_polarized_resolution_without_support_uses_ordinary_axioms;
       ])
    ]
