open Alcotest
open Prover_lib
open Types

let var x = Var x
let const c = Fun (c, [])
let fun_ f args = Fun (f, args)

let atom p args = { pred = p; args = args }
let pos a = Pos a
let neg a = Neg a

let test_match_terms_1 () =
  (* p(X) matches p(a) *)
  let s = Match.match_terms (var "X") (const "a") StringMap.empty in
  check string "X -> a" "a" (Pretty.string_of_term (StringMap.find "X" s))

let test_match_terms_2 () =
  (* p(a) does not match p(X) *)
  try
    let _ = Match.match_terms (const "a") (var "X") StringMap.empty in
    fail "Should raise Not_matchable"
  with Match.Not_matchable -> ()

let test_match_terms_3 () =
  (* f(X, X) matches f(a, a) *)
  let s = Match.match_terms (fun_ "f" [var "X"; var "X"]) (fun_ "f" [const "a"; const "a"]) StringMap.empty in
  check string "X -> a" "a" (Pretty.string_of_term (StringMap.find "X" s))

let test_match_terms_4 () =
  (* f(X, X) does not match f(a, b) *)
  try
    let _ = Match.match_terms (fun_ "f" [var "X"; var "X"]) (fun_ "f" [const "a"; const "b"]) StringMap.empty in
    fail "Should raise Not_matchable"
  with Match.Not_matchable -> ()

let test_match_terms_deep () =
  (* f(g(X, a), h(Y)) matches f(g(b, a), h(c)) *)
  let t1 = fun_ "f" [fun_ "g" [var "X"; const "a"]; fun_ "h" [var "Y"]] in
  let t2 = fun_ "f" [fun_ "g" [const "b"; const "a"]; fun_ "h" [const "c"]] in
  let s = Match.match_terms t1 t2 StringMap.empty in
  check string "X -> b" "b" (Pretty.string_of_term (StringMap.find "X" s));
  check string "Y -> c" "c" (Pretty.string_of_term (StringMap.find "Y" s))

let test_match_terms_deep_fail () =
  (* f(g(X, X), h(Y)) does not match f(g(b, a), h(c)) *)
  let t1 = fun_ "f" [fun_ "g" [var "X"; var "X"]; fun_ "h" [var "Y"]] in
  let t2 = fun_ "f" [fun_ "g" [const "b"; const "a"]; fun_ "h" [const "c"]] in
  try
    let _ = Match.match_terms t1 t2 StringMap.empty in
    fail "Should raise Not_matchable"
  with Match.Not_matchable -> ()

let test_subsumes_1 () =
  (* P(X) subsumes P(a) *)
  let c1 = [ pos (atom "p" [var "X"]) ] in
  let c2 = [ pos (atom "p" [const "a"]) ] in
  check bool "subsumes" true (Clause.subsumes c1 c2)

let test_subsumes_2 () =
  (* P(a) does not subsume P(X) *)
  let c1 = [ pos (atom "p" [const "a"]) ] in
  let c2 = [ pos (atom "p" [var "X"]) ] in
  check bool "not subsumes" false (Clause.subsumes c1 c2)

let test_subsumes_3 () =
  (* P(X) v Q(Y) subsumes P(a) v Q(b) v R(c) *)
  let c1 = [ pos (atom "p" [var "X"]); pos (atom "q" [var "Y"]) ] in
  let c2 = [ pos (atom "p" [const "a"]); pos (atom "q" [const "b"]); pos (atom "r" [const "c"]) ] in
  check bool "subsumes" true (Clause.subsumes c1 c2)

let test_subsumes_4 () =
  (* P(X, X) does not subsume P(a, b) *)
  let c1 = [ pos (atom "p" [var "X"; var "X"]) ] in
  let c2 = [ pos (atom "p" [const "a"; const "b"]) ] in
  check bool "not subsumes" false (Clause.subsumes c1 c2)

let test_subsumes_5 () =
  (* Subset: P(a) subsumes P(a) v Q(b) *)
  let c1 = [ pos (atom "p" [const "a"]) ] in
  let c2 = [ pos (atom "p" [const "a"]); pos (atom "q" [const "b"]) ] in
  check bool "subsumes subset" true (Clause.subsumes c1 c2)

let test_subsumes_order_independent () =
  (* P(X) v Q(Y) subsumes Q(b) v P(a) *)
  let c1 = [ pos (atom "p" [var "X"]); pos (atom "q" [var "Y"]) ] in
  let c2 = [ pos (atom "q" [const "b"]); pos (atom "p" [const "a"]) ] in
  check bool "subsumes order independent" true (Clause.subsumes c1 c2)

let test_subsumes_multiplicity () =
  (* P(X) v P(Y) does NOT currently subsume P(a) in our simple engine.
     Subsumption is one-to-one over target literals. *)
  let c1 = [ pos (atom "p" [var "X"]); pos (atom "p" [var "Y"]) ] in
  let c2 = [ pos (atom "p" [const "a"]) ] in
  check bool "subsumes multiplicity (known limitation: greedy 1-to-1)" false (Clause.subsumes c1 c2)

let test_subsumes_resolution_1 () =
  (* P(X) subsumption-resolves ~P(a) v Q(b) -> Q(b) *)
  let c1 = [ pos (atom "p" [var "X"]) ] in
  let c2 = [ neg (atom "p" [const "a"]); pos (atom "q" [const "b"]) ] in
  match Clause.subsumption_resolution c1 c2 with
  | Some res ->
      let expected = [ pos (atom "q" [const "b"]) ] in
      check string "resolved" (Pretty.string_of_clause expected) (Pretty.string_of_clause res)
  | None -> fail "Should resolve"

let test_subsumes_resolution_2 () =
  (* P(a) v Q(b) subsumption-resolves ~P(a) v Q(b) v R(c) -> Q(b) v R(c) *)
  let c1 = [ pos (atom "p" [const "a"]); pos (atom "q" [const "b"]) ] in
  let c2 = [ neg (atom "p" [const "a"]); pos (atom "q" [const "b"]); pos (atom "r" [const "c"]) ] in
  match Clause.subsumption_resolution c1 c2 with
  | Some res ->
      let expected = [ pos (atom "q" [const "b"]); pos (atom "r" [const "c"]) ] in
      check string "resolved" (Pretty.string_of_clause expected) (Pretty.string_of_clause res)
  | None -> fail "Should resolve"

let test_fast_condensation_removes_instance_generalization () =
  (* P(X) v P(a) -> P(a), because P(a) is an instance of P(X). *)
  let c = [ pos (atom "p" [var "X"]); pos (atom "p" [const "a"]) ] in
  match Clause.fast_condense_clause c with
  | Some res ->
      let expected = [ pos (atom "p" [const "a"]) ] in
      check string "condensed" (Pretty.string_of_clause expected) (Pretty.string_of_clause res)
  | None -> fail "Should keep a condensed clause"

let test_fast_condensation_preserves_shared_vars () =
  (* P(X) cannot be deleted using P(a) if X is shared with Q(X). *)
  let c = [ pos (atom "p" [var "X"]); pos (atom "p" [const "a"]); pos (atom "q" [var "X"]) ] in
  match Clause.fast_condense_clause c with
  | Some res -> check string "unchanged" (Pretty.string_of_clause (Clause.normalize_clause c)) (Pretty.string_of_clause res)
  | None -> fail "Should keep the non-tautological clause"

let test_fast_condensation_allows_private_vars () =
  (* Only Y is private to P(X,Y), so P(X,Y) can be deleted using P(X,a). *)
  let c = [ pos (atom "p" [var "X"; var "Y"]); pos (atom "p" [var "X"; const "a"]); pos (atom "q" [var "X"]) ] in
  match Clause.fast_condense_clause c with
  | Some res ->
      let expected = [ pos (atom "p" [var "X"; const "a"]); pos (atom "q" [var "X"]) ] in
      check string "condensed private var" (Pretty.string_of_clause (Clause.normalize_clause expected)) (Pretty.string_of_clause res)
  | None -> fail "Should keep a condensed clause"

let () =
  run "Match and Subsumption"
    [
      ("matching",
       [
         test_case "match var to const" `Quick test_match_terms_1;
         test_case "no match const to var" `Quick test_match_terms_2;
         test_case "match identical vars to identical consts" `Quick test_match_terms_3;
         test_case "no match identical vars to diff consts" `Quick test_match_terms_4;
         test_case "match deep terms" `Quick test_match_terms_deep;
         test_case "fail match deep terms on var conflict" `Quick test_match_terms_deep_fail;
       ]);
      ("subsumption",
       [
         test_case "general subsumes specific" `Quick test_subsumes_1;
         test_case "specific does not subsume general" `Quick test_subsumes_2;
         test_case "general subset subsumes specific superset" `Quick test_subsumes_3;
         test_case "identical vars enforce equality in subsumption" `Quick test_subsumes_4;
         test_case "subset subsumes superset" `Quick test_subsumes_5;
         test_case "subsumption is order independent" `Quick test_subsumes_order_independent;
         test_case "subsumption rejects multiplicity reduction" `Quick test_subsumes_multiplicity;
       ]);
      ("fast_condensation",
       [
         test_case "remove general literal with instance" `Quick test_fast_condensation_removes_instance_generalization;
         test_case "preserve shared variables" `Quick test_fast_condensation_preserves_shared_vars;
         test_case "allow private variable bindings" `Quick test_fast_condensation_allows_private_vars;
       ]);
      ("subsumption_resolution",
       [
         test_case "unit subsumption resolution" `Quick test_subsumes_resolution_1;
         test_case "multi-literal subsumption resolution" `Quick test_subsumes_resolution_2;
       ])
    ]
