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

let test_subsumes_keeps_clause_variables_apart () =
  (* Variables in independently quantified clauses are not shared.  In
     particular, matching the first X against the target X must not make the
     second occurrence free to match Y. *)
  let c1 = [ pos (atom "p" [var "X"; var "X"]) ] in
  let c2 = [ pos (atom "p" [var "X"; var "Y"]) ] in
  check bool "independent variables stay apart" false (Clause.subsumes c1 c2)

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

let test_subsumption_resolution_keeps_variables_apart () =
  (* These parents entail R(X,X), not the universally stronger R(X,Y). *)
  let c1 = [ pos (atom "p" [var "X"; var "X"]) ] in
  let c2 =
    [ neg (atom "p" [var "X"; var "Y"]);
      pos (atom "r" [var "X"; var "Y"]) ]
  in
  check bool "reject variable-capturing simplification" true
    (Clause.subsumption_resolution c1 c2 = None)

let test_subsumption_resolution_preserves_resolving_substitution () =
  (* Resolving P(X) against ~P(Y) fixes X to the rigid target variable Y.
     Q(X) must then match Q(Y), not Q(a).  Deleting ~P(Y) here would derive
     Q(a) v R(Y), which is not entailed by these two clauses. *)
  let c1 =
    [ pos (atom "p" [var "X"]);
      pos (atom "q" [var "X"]) ]
  in
  let c2 =
    [ neg (atom "p" [var "Y"]);
      pos (atom "q" [const "a"]);
      pos (atom "r" [var "Y"]) ]
  in
  check bool "keep the resolving substitution across the subset match" true
    (Clause.subsumption_resolution c1 c2 = None)

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

let test_full_condensation_applies_substitution_to_whole_clause () =
  (* Fast condensation cannot delete P(X) or Q(X) independently because X is
     shared, but full condensation can instantiate the whole clause. *)
  let c = [ pos (atom "p" [var "X"]); pos (atom "q" [var "X"]); pos (atom "p" [const "a"]); pos (atom "q" [const "a"]) ] in
  match Clause.full_condense_clause c with
  | Some res ->
      let expected = [ pos (atom "p" [const "a"]); pos (atom "q" [const "a"]) ] in
      check string "full condensed" (Pretty.string_of_clause (Clause.normalize_clause expected)) (Pretty.string_of_clause res)
  | None -> fail "Should keep a condensed clause"

let test_full_condensation_rejects_cyclic_substitution () =
  (* Matching P(X) against P(f(X)) would create X -> f(X), which must not be
     applied because it would make substitution application diverge. *)
  let c = [ pos (atom "p" [var "X"]); pos (atom "p" [fun_ "f" [var "X"]]) ] in
  match Clause.full_condense_clause c with
  | Some res -> check string "unchanged cyclic" (Pretty.string_of_clause (Clause.normalize_clause c)) (Pretty.string_of_clause res)
  | None -> fail "Should keep the non-tautological clause"

let clause_holds_in_two_element_model ~a_value ~b_value ~pred_masks clause =
  let vars = Clause.vars_of_clause clause |> StringSet.elements in
  let assignment = Hashtbl.create (List.length vars) in
  let rec eval_term = function
    | Var v -> Hashtbl.find assignment v
    | Fun ("a", []) -> a_value
    | Fun ("b", []) -> b_value
    | Fun (name, []) -> failwith ("unexpected audit constant " ^ name)
    | Fun (name, _) -> failwith ("unexpected audit function " ^ name)
  in
  let eval_atom atom =
    match atom.pred, atom.args with
    | "=", [ left; right ] -> eval_term left = eval_term right
    | pred, [ arg ] ->
        let mask = List.assoc pred pred_masks in
        mask land (1 lsl eval_term arg) <> 0
    | pred, _ -> failwith ("unexpected audit predicate " ^ pred)
  in
  let eval_literal = function
    | Pos atom -> eval_atom atom
    | Neg atom -> not (eval_atom atom)
  in
  let clause_holds_for_assignment () = List.exists eval_literal clause in
  let rec all_assignments = function
    | [] -> clause_holds_for_assignment ()
    | v :: rest ->
        Hashtbl.replace assignment v 0;
        let at_zero = all_assignments rest in
        Hashtbl.replace assignment v 1;
        at_zero && all_assignments rest
  in
  all_assignments vars

let consequence_holds_in_all_two_element_models parents conclusion =
  let exception Countermodel in
  try
    for a_value = 0 to 1 do
      for b_value = 0 to 1 do
        for p_mask = 0 to 3 do
          for q_mask = 0 to 3 do
            for r_mask = 0 to 3 do
              let pred_masks = [ "p", p_mask; "q", q_mask; "r", r_mask ] in
              if List.for_all
                   (clause_holds_in_two_element_model
                      ~a_value ~b_value ~pred_masks)
                   parents
                 && not
                      (clause_holds_in_two_element_model
                         ~a_value ~b_value ~pred_masks conclusion)
              then raise Countermodel
            done
          done
        done
      done
    done;
    true
  with Countermodel -> false

let test_generated_subsumption_resolutions_have_no_small_countermodel () =
  let state = Random.State.make [| 0x51A7; 0xC0DE |] in
  let terms = [| var "X"; var "Y"; const "a"; const "b" |] in
  let predicates = [| "p"; "q"; "r" |] in
  let random_literal () =
    let atom =
      atom
        predicates.(Random.State.int state (Array.length predicates))
        [ terms.(Random.State.int state (Array.length terms)) ]
    in
    if Random.State.bool state then pos atom else neg atom
  in
  let random_clause max_len =
    List.init (1 + Random.State.int state max_len) (fun _ -> random_literal ())
    |> Clause.normalize_clause
  in
  for _ = 1 to 2_000 do
    let source = random_clause 3 in
    let target = random_clause 4 in
    match Clause.subsumption_resolution source target with
    | None -> ()
    | Some reduced ->
        if not
             (consequence_holds_in_all_two_element_models
                [ source; target ] reduced)
        then
          failf
            "finite countermodel for subsumption resolution: (%s), (%s) => (%s)"
            (Pretty.string_of_clause source)
            (Pretty.string_of_clause target)
            (Pretty.string_of_clause reduced)
  done

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
         test_case "subsumption separates clause variables" `Quick test_subsumes_keeps_clause_variables_apart;
       ]);
      ("fast_condensation",
       [
         test_case "remove general literal with instance" `Quick test_fast_condensation_removes_instance_generalization;
         test_case "preserve shared variables" `Quick test_fast_condensation_preserves_shared_vars;
         test_case "allow private variable bindings" `Quick test_fast_condensation_allows_private_vars;
       ]);
      ("full_condensation",
       [
         test_case "instantiate whole clause" `Quick test_full_condensation_applies_substitution_to_whole_clause;
         test_case "reject cyclic substitution" `Quick test_full_condensation_rejects_cyclic_substitution;
       ]);
      ("subsumption_resolution",
       [
         test_case "unit subsumption resolution" `Quick test_subsumes_resolution_1;
         test_case "multi-literal subsumption resolution" `Quick test_subsumes_resolution_2;
         test_case "subsumption resolution separates variables" `Quick test_subsumption_resolution_keeps_variables_apart;
         test_case "subsumption resolution preserves its substitution" `Quick test_subsumption_resolution_preserves_resolving_substitution;
         test_case "generated results have no two-element countermodel" `Quick test_generated_subsumption_resolutions_have_no_small_countermodel;
       ])
    ]
