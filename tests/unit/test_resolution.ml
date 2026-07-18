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

let test_binary_resolution_preserves_nested_argument_order () =
  Clause.reset_fresh_counter ();
  let implication x y = fun_ "strict_implies" [ x; y ] in
  let possibly x = fun_ "possibly" [ x ] in
  let conjunction x y = fun_ "and" [ x; y ] in
  let theorem x = atom "is_a_theorem" [ x ] in
  let left =
    [
      pos (theorem (var "Y"));
      neg
        (theorem
           (implication
              (implication (var "X") (possibly (var "X")))
              (var "Y")));
    ]
  in
  let right =
    [ pos (theorem (implication (var "Z") (conjunction (var "Z") (var "Z")))) ]
  in
  match Resolution.test_resolve Unrestricted left right with
  | [ [ Pos actual ] ] ->
      let expected_inner = implication (var "V0") (possibly (var "V0")) in
      let expected = theorem (conjunction expected_inner expected_inner) in
      check string "nested resolvent" (Pretty.string_of_atom expected) (Pretty.string_of_atom actual)
  | _ -> fail "Nested resolution should generate exactly one unit resolvent"

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

let test_indexed_superposition_keeps_target_variable_binding () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let axioms =
    [
      [ eq (fun_ "f" [ const "a" ]) (const "b") ];
      [ neg (atom "p" [ fun_ "f" [ var "X" ] ]); pos (atom "q" [ var "X" ]) ];
      [ pos (atom "p" [ const "b" ]) ];
      [ neg (atom "q" [ const "c" ]) ];
      [ neq (const "a") (const "c") ];
    ]
  in
  let res =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 500 }
      ~expensive_simplifications:false
      ~mode:Unrestricted
      ~axioms
      ~support:[]
      ()
  in
  match res.stop_reason with
  | Saturation -> ()
  | Refutation_found _ ->
      fail "Indexed superposition must apply the unifier to the renamed target clause"
  | Time_limit | Clause_limit -> fail "Soundness regression should saturate quickly"

let test_demodulation_standardizes_rule_variables_apart () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let sort x y = fun_ "s" [ x; y ] in
  let bool_sort = const "bool" in
  let truth = const "true" in
  let cond p x y = fun_ "cond" [ p; x; y ] in
  let source =
    [
      eq
        (sort (var "A") (cond (sort bool_sort truth) (sort (var "A") (var "X")) (sort (var "A") (var "Y"))))
        (sort (var "A") (var "X"));
    ]
  in
  let symmetry =
    [
      neq (sort (var "B") (var "U")) (sort (var "B") (var "V"));
      eq (sort (var "B") (var "V")) (sort (var "B") (var "U"));
    ]
  in
  let bool_distinct =
    [
      neq (sort bool_sort (var "Z")) (sort bool_sort (const "false"));
      neq (sort bool_sort (var "Z")) (sort bool_sort truth);
    ]
  in
  let result =
    Resolution.run_resolution_sos
      ~limits:
        {
          Resolution.time_limit_s = Some 1.0;
          max_generated_clauses = Some 1000;
        }
      ~expensive_simplifications:true
      ~mode:Unrestricted
      ~axioms:[ source; symmetry; bool_distinct ]
      ~support:[]
      ()
  in
  match result.stop_reason with
  | Saturation -> ()
  | Refutation_found _ ->
      fail "Demodulator variables must not capture variables in rewritten clauses"
  | Time_limit | Clause_limit -> ()

let test_legacy_demodulation_standardizes_rule_variables_apart () =
  Legacy_resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let sort x y = fun_ "s" [ x; y ] in
  let bool_sort = const "bool" in
  let truth = const "true" in
  let source =
    [
      eq
        (sort
           (var "A")
           (fun_
              "cond"
              [
                sort bool_sort truth;
                sort (var "A") (var "X");
                sort (var "A") (var "Y");
              ]))
        (sort (var "A") (var "X"));
    ]
  in
  let symmetry =
    [
      neq (sort (var "B") (var "U")) (sort (var "B") (var "V"));
      eq (sort (var "B") (var "V")) (sort (var "B") (var "U"));
    ]
  in
  let bool_distinct =
    [
      neq (sort bool_sort (var "Z")) (sort bool_sort (const "false"));
      neq (sort bool_sort (var "Z")) (sort bool_sort truth);
    ]
  in
  let result =
    Legacy_resolution.run_resolution_sos
      ~limits:
        {
          Legacy_resolution.time_limit_s = Some 1.0;
          max_generated_clauses = Some 100;
        }
      ~mode:Legacy_resolution.Unrestricted
      ~axioms:[ source; symmetry; bool_distinct ]
      ~support:[]
      ()
  in
  match result.stop_reason with
  | Legacy_resolution.Saturation | Legacy_resolution.Clause_limit -> ()
  | Legacy_resolution.Refutation_found _ ->
      fail "Legacy demodulator variables must not capture rewritten clause variables"
  | Legacy_resolution.Time_limit -> ()

let test_legacy_indexed_superposition_keeps_target_variable_binding () =
  Legacy_resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let axioms =
    [
      [ eq (fun_ "f" [ const "a" ]) (const "b") ];
      [ neg (atom "p" [ fun_ "f" [ var "X" ] ]); pos (atom "q" [ var "X" ]) ];
      [ pos (atom "p" [ const "b" ]) ];
      [ neg (atom "q" [ const "c" ]) ];
      [ neq (const "a") (const "c") ];
    ]
  in
  let res =
    Legacy_resolution.run_resolution_sos
      ~limits:{ Legacy_resolution.default_limits with max_generated_clauses = Some 500 }
      ~mode:Legacy_resolution.Unrestricted
      ~axioms
      ~support:[]
      ()
  in
  match res.stop_reason with
  | Legacy_resolution.Saturation -> ()
  | Legacy_resolution.Refutation_found _ ->
      fail "Legacy indexed superposition must use consistently renamed terms"
  | Legacy_resolution.Time_limit | Legacy_resolution.Clause_limit ->
      fail "Legacy soundness regression should saturate quickly"

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

let test_polarized_atom_rewrite_does_not_reverse_implication () =
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
      fail "A one-way implication must not rewrite a negative antecedent"
  | Time_limit | Clause_limit -> fail "Polarized soundness test should saturate quickly"

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

let test_avatar_cannot_be_enabled_in_release () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  Unix.putenv "VIP_AVATAR_SPLITTING" "1";
  Unix.putenv "VIP_ENABLE_EXPERIMENTAL_AVATAR" "1";
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "VIP_AVATAR_SPLITTING" "";
      Unix.putenv "VIP_ENABLE_EXPERIMENTAL_AVATAR" "")
    (fun () ->
      let res =
        Resolution.run_resolution_sos
          ~limits:{ Resolution.default_limits with max_generated_clauses = Some 100 }
          ~expensive_simplifications:true
          ~mode:Unrestricted
          ~axioms:[ [ pos (atom "p" []) ] ]
          ~support:[ [ neg (atom "p" []) ] ]
          ()
      in
      check bool "AVATAR hard-disabled in release" false res.stats.avatar_enabled)

let clause_holds_in_model ~constants ~f_map ~predicates clause =
  let variables = Clause.vars_of_clause clause |> StringSet.elements in
  let assignment = Hashtbl.create (List.length variables) in
  let rec eval_term = function
    | Var v -> Hashtbl.find assignment v
    | Fun (name, []) -> List.assoc name constants
    | Fun ("f", [ arg ]) -> List.nth f_map (eval_term arg)
    | Fun (name, _) -> failwith ("unexpected audit function " ^ name)
  in
  let eval_atom a =
    match a.pred, a.args with
    | "=", [ left; right ] -> eval_term left = eval_term right
    | pred, [ arg ] ->
        let mask = List.assoc pred predicates in
        mask land (1 lsl eval_term arg) <> 0
    | pred, _ -> failwith ("unexpected audit predicate " ^ pred)
  in
  let eval_literal = function
    | Pos a -> eval_atom a
    | Neg a -> not (eval_atom a)
  in
  let rec all_assignments = function
    | [] -> List.exists eval_literal clause
    | v :: rest ->
        Hashtbl.replace assignment v 0;
        let at_zero = all_assignments rest in
        Hashtbl.replace assignment v 1;
        at_zero && all_assignments rest
  in
  all_assignments variables

let test_saturation_preserves_all_two_element_models () =
  Resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let axioms =
    [
      [ eq (fun_ "f" [ const "a" ]) (const "b") ];
      [ neg (atom "p" [ fun_ "f" [ var "X" ] ]); pos (atom "q" [ var "X" ]) ];
      [ pos (atom "p" [ const "b" ]) ];
      [ neg (atom "q" [ const "c" ]) ];
      [ neq (const "a") (const "c") ];
    ]
  in
  let result =
    Resolution.run_resolution_sos
      ~limits:{ Resolution.default_limits with max_generated_clauses = Some 500 }
      ~expensive_simplifications:true
      ~mode:Unrestricted
      ~axioms
      ~support:[]
      ()
  in
  let satisfying_models = ref 0 in
  for a = 0 to 1 do
    for b = 0 to 1 do
      for c = 0 to 1 do
        for f0 = 0 to 1 do
          for f1 = 0 to 1 do
            for p = 0 to 3 do
              for q = 0 to 3 do
                let constants = [ "a", a; "b", b; "c", c ] in
                let f_map = [ f0; f1 ] in
                let predicates = [ "p", p; "q", q ] in
                let holds =
                  clause_holds_in_model ~constants ~f_map ~predicates
                in
                if List.for_all holds axioms then begin
                  incr satisfying_models;
                  List.iter
                    (fun (d : Resolution.derived) ->
                      if not (holds d.clause_d) then
                        failf
                          "derived clause %d (%s) has a two-element countermodel: %s"
                          d.id
                          d.rule
                          (Pretty.string_of_clause d.clause_d))
                    result.derivation
                end
              done
            done
          done
        done
      done
    done
  done;
  check bool "input has audited models" true (!satisfying_models > 0)

let test_legacy_saturation_preserves_all_two_element_models () =
  Legacy_resolution.reset_id_counter ();
  Clause.reset_fresh_counter ();
  let axioms =
    [
      [ eq (fun_ "f" [ const "a" ]) (const "b") ];
      [ neg (atom "p" [ fun_ "f" [ var "X" ] ]); pos (atom "q" [ var "X" ]) ];
      [ pos (atom "p" [ const "b" ]) ];
      [ neg (atom "q" [ const "c" ]) ];
      [ neq (const "a") (const "c") ];
    ]
  in
  let result =
    Legacy_resolution.run_resolution_sos
      ~limits:{ Legacy_resolution.default_limits with max_generated_clauses = Some 500 }
      ~mode:Legacy_resolution.Unrestricted
      ~axioms
      ~support:[]
      ()
  in
  let satisfying_models = ref 0 in
  for a = 0 to 1 do
    for b = 0 to 1 do
      for c = 0 to 1 do
        for f0 = 0 to 1 do
          for f1 = 0 to 1 do
            for p = 0 to 3 do
              for q = 0 to 3 do
                let constants = [ "a", a; "b", b; "c", c ] in
                let f_map = [ f0; f1 ] in
                let predicates = [ "p", p; "q", q ] in
                let holds =
                  clause_holds_in_model ~constants ~f_map ~predicates
                in
                if List.for_all holds axioms then begin
                  incr satisfying_models;
                  List.iter
                    (fun (d : Legacy_resolution.derived) ->
                      if not (holds d.clause_d) then
                        failf
                          "legacy clause %d (%s) has a two-element countermodel: %s"
                          d.id
                          d.rule
                          (Pretty.string_of_clause d.clause_d))
                    result.derivation
                end
              done
            done
          done
        done
      done
    done
  done;
  check bool "legacy input has audited models" true (!satisfying_models > 0)

let () =
  run "Resolution and Factoring"
    [
      ("resolution",
       [
         test_case "binary resolution unrestricted" `Quick test_binary_resolution_unrestricted;
         test_case
           "binary resolution preserves nested argument order"
           `Quick
           test_binary_resolution_preserves_nested_argument_order;
         test_case "equality resolution" `Quick test_equality_resolution;
       ]);
      ("factoring",
       [
         test_case "factoring unrestricted" `Quick test_factoring_unrestricted;
         test_case "equality factoring" `Quick test_equality_factoring;
         test_case
           "indexed superposition preserves target bindings"
           `Quick
           test_indexed_superposition_keeps_target_variable_binding;
         test_case
           "demodulation standardizes rule variables apart"
           `Quick
           test_demodulation_standardizes_rule_variables_apart;
         test_case
           "legacy indexed superposition preserves target bindings"
           `Quick
           test_legacy_indexed_superposition_keeps_target_variable_binding;
         test_case
           "legacy demodulation standardizes rule variables apart"
           `Quick
           test_legacy_demodulation_standardizes_rule_variables_apart;
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
           "atom rewrite preserves negative antecedents"
           `Quick
           test_polarized_atom_rewrite_does_not_reverse_implication;
         test_case
           "ordinary axioms without support"
           `Quick
           test_polarized_resolution_without_support_uses_ordinary_axioms;
         test_case
           "AVATAR cannot be enabled in release"
           `Quick
           test_avatar_cannot_be_enabled_in_release;
         test_case
           "saturation preserves all two-element models"
           `Quick
           test_saturation_preserves_all_two_element_models;
         test_case
           "legacy saturation preserves all two-element models"
           `Quick
           test_legacy_saturation_preserves_all_two_element_models;
       ])
    ]
