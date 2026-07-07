open Alcotest
open Prover_lib
open Types
open Fof

let test_simple_clausification () =
  Clausify.reset_fresh_state ();
  let f =
    Forall
      ( [ "X" ],
        Imp
          ( Atom { pred = "p"; args = [ Var "X" ] },
            Atom { pred = "q"; args = [ Var "X" ] } ) )
  in
  let clauses = Clausify.clausify_formula f in
  check bool "has clauses" true (List.length clauses > 0)

let clause_strings clauses =
  clauses |> List.map Pretty.string_of_clause |> List.sort String.compare

let test_exists_without_universals_uses_skolem_constant () =
  Clausify.reset_fresh_state ();
  let f =
    Exists ([ "X" ], Atom { pred = "p"; args = [ Var "X" ] })
  in
  check
    (list string)
    "existential constant"
    [ "p(sk1)" ]
    (clause_strings (Clausify.clausify_formula f))

let test_exists_under_forall_uses_skolem_function () =
  Clausify.reset_fresh_state ();
  let f =
    Forall
      ( [ "X" ],
        Exists
          ( [ "Y" ],
            Atom { pred = "p"; args = [ Var "X"; Var "Y" ] } ) )
  in
  check
    (list string)
    "existential depends on universal"
    [ "p(V0,sk1(V0))" ]
    (clause_strings (Clausify.clausify_formula f))

let test_nested_existential_depends_on_all_visible_universals () =
  Clausify.reset_fresh_state ();
  let f =
    Forall
      ( [ "X" ],
        Exists
          ( [ "Y" ],
            Forall
              ( [ "Z" ],
                Exists
                  ( [ "W" ],
                    Atom
                      {
                        pred = "p";
                        args = [ Var "X"; Var "Y"; Var "Z"; Var "W" ];
                      } ) ) ) )
  in
  check
    (list string)
    "nested visible universals"
    [ "p(V0,sk1(V0),V1,sk2(V0,V1))" ]
    (clause_strings (Clausify.clausify_formula f))

let test_same_existential_block_gets_independent_skolem_terms () =
  Clausify.reset_fresh_state ();
  let f =
    Forall
      ( [ "X" ],
        Exists
          ( [ "Y"; "Z" ],
            Atom { pred = "p"; args = [ Var "X"; Var "Y"; Var "Z" ] } ) )
  in
  check
    (list string)
    "same block independent skolems"
    [ "p(V0,sk1(V0),sk2(V0))" ]
    (clause_strings (Clausify.clausify_formula f))

let test_standardization_handles_shadowed_variables_before_skolemization () =
  Clausify.reset_fresh_state ();
  let f =
    Forall
      ( [ "X" ],
        Or
          ( Atom { pred = "p"; args = [ Var "X" ] },
            Exists ([ "X" ], Atom { pred = "q"; args = [ Var "X" ] }) ) )
  in
  check
    (list string)
    "shadowed variable"
    [ "p(V0) | q(sk1(V0))" ]
    (clause_strings (Clausify.clausify_formula f))

let test_skolem_names_avoid_existing_function_symbols_in_formula () =
  Clausify.reset_fresh_state ();
  let f =
    And
      ( Atom { pred = "p"; args = [ Fun ("sk1", []) ] },
        Exists ([ "X" ], Atom { pred = "q"; args = [ Var "X" ] }) )
  in
  check
    (list string)
    "formula symbol collision"
    [ "p(sk1)"; "q(sk2)" ]
    (clause_strings (Clausify.clausify_formula f))

let test_skolem_names_avoid_existing_function_symbols_across_inputs () =
  Clausify.reset_fresh_state ();
  let existential_input =
    Input_fof
      {
        name = "exists_q";
        source_file = None;
        role = "axiom";
        formula = Exists ([ "X" ], Atom { pred = "q"; args = [ Var "X" ] });
      }
  in
  let later_user_symbol =
    Input_fof
      {
        name = "uses_sk1";
        source_file = None;
        role = "axiom";
        formula = Atom { pred = "p"; args = [ Fun ("sk1", []) ] };
      }
  in
  let clauses, _ =
    Clausify.clauses_of_input_with_report [ existential_input; later_user_symbol ]
  in
  check
    (list string)
    "input-wide symbol collision"
    [ "p(sk1)"; "q(sk2)" ]
    (clause_strings clauses)

let with_env name value f =
  let old = Sys.getenv_opt name in
  Option.iter (fun v -> Unix.putenv name v) value;
  Fun.protect
    ~finally:(fun () ->
      match old with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f

let with_envs vars f =
  let old = List.map (fun (name, _) -> (name, Sys.getenv_opt name)) vars in
  List.iter (fun (name, value) -> Unix.putenv name value) vars;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, value) ->
          match value with
          | Some v -> Unix.putenv name v
          | None -> Unix.putenv name "")
        old)
    f

let test_one_way_definition () =
  Clausify.reset_fresh_state ();
  let input =
    Input_fof
      {
        name = "def_p";
        source_file = None;
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Iff
                ( Atom { pred = "p"; args = [ Var "X" ] },
                  Atom { pred = "q"; args = [ Var "X" ] } ) );
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ input ] in
  let one_way =
    with_env "VIP_ONE_WAY_DEFINITIONS" (Some "1") (fun () ->
      fst (Clausify.clauses_of_input_with_report [ input ]))
  in
  check int "baseline clauses" 2 (List.length baseline);
  check int "one-way clauses" 1 (List.length one_way)

let test_one_way_trace_metadata () =
  let def =
    Input_fof
      {
        name = "def_p";
        source_file = None;
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Iff
                ( Atom { pred = "p"; args = [ Var "X" ] },
                  Atom { pred = "q"; args = [ Var "X" ] } ) );
      }
  in
  let use =
    Input_fof
      {
        name = "use_p";
        source_file = None;
        role = "axiom";
        formula = Atom { pred = "p"; args = [ Fun ("a", []) ] };
      }
  in
  let traced =
    with_env "VIP_ONE_WAY_DEFINITIONS" (Some "1") (fun () ->
      Clausify.partition_input_clauses_with_trace [ def; use ])
  in
  let one_way =
    List.filter (fun origin -> origin.Clausify.one_way) traced.trace.origins
  in
  check int "one-way traced clauses" 1 (List.length one_way);
  check
    (list string)
    "one-way traced clause"
    [ "~p(V0) | q(V0)" ]
    (List.map (fun origin -> Pretty.string_of_clause origin.Clausify.clause) one_way)

let test_guarded_one_way_definition () =
  Clausify.reset_fresh_state ();
  let input =
    Input_fof
      {
        name = "guarded_def_p";
        source_file = None;
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Imp
                ( Atom { pred = "r"; args = [ Var "X" ] },
                  Iff
                    ( Atom { pred = "p"; args = [ Var "X" ] },
                      Atom { pred = "q"; args = [ Var "X" ] } ) ) );
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ input ] in
  let one_way =
    with_env "VIP_ONE_WAY_DEFINITIONS" (Some "1") (fun () ->
      fst (Clausify.clauses_of_input_with_report [ input ]))
  in
  check int "baseline guarded clauses" 2 (List.length baseline);
  check int "guarded one-way clauses" 1 (List.length one_way);
  check
    (list string)
    "guarded one-way clause"
    [ "~p(V0) | q(V0) | ~r(V0)" ]
    (List.map Pretty.string_of_clause one_way)

let test_dmt_definition_expansion () =
  Clausify.reset_fresh_state ();
  let def =
    Input_fof
      {
        name = "def_p";
        source_file = None;
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Iff
                ( Atom { pred = "p"; args = [ Var "X" ] },
                  Atom { pred = "q"; args = [ Var "X" ] } ) );
      }
  in
  let use =
    Input_fof
      {
        name = "use_p";
        source_file = None;
        role = "axiom";
        formula = Atom { pred = "p"; args = [ Fun ("a", []) ] };
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ def; use ] in
  let expanded =
    with_envs
      [ ("VIP_DMT_EXPAND_DEFINITIONS", "1"); ("VIP_ONE_WAY_DEFINITIONS", "0") ]
      (fun () -> fst (Clausify.clauses_of_input_with_report [ def; use ]))
  in
  check int "baseline keeps definition" 3 (List.length baseline);
  check int "expanded removes definition" 1 (List.length expanded);
  check
    (list string)
    "rewritten clause"
    [ "q(a)" ]
    (List.map Pretty.string_of_clause expanded)

let test_dmt_collects_conjoined_definitions () =
  let defs =
    Input_fof
      {
        name = "defs";
        source_file = None;
        role = "axiom";
        formula =
          And
            ( Forall
                ( [ "X" ],
                  Iff
                    ( Atom { pred = "p"; args = [ Var "X" ] },
                      Atom { pred = "q"; args = [ Var "X" ] } ) ),
              Forall
                ( [ "Y" ],
                  Iff
                    ( Atom { pred = "r"; args = [ Var "Y" ] },
                      Atom { pred = "s"; args = [ Var "Y" ] } ) ) );
      }
  in
  let use =
    Input_fof
      {
        name = "use_defs";
        source_file = None;
        role = "axiom";
        formula =
          And
            ( Atom { pred = "p"; args = [ Fun ("a", []) ] },
              Atom { pred = "r"; args = [ Fun ("b", []) ] } );
      }
  in
  let expanded =
    with_envs
      [ ("VIP_DMT_EXPAND_DEFINITIONS", "1"); ("VIP_ONE_WAY_DEFINITIONS", "0") ]
      (fun () -> fst (Clausify.clauses_of_input_with_report [ defs; use ]))
  in
  check
    (list string)
    "rewritten conjoined definitions"
    [ "q(a)"; "s(b)" ]
    (List.map Pretty.string_of_clause expanded)

let () =
  run "clausify"
    [
      ("fof",
       [
         test_case "simple implication" `Quick test_simple_clausification;
         test_case "existential skolem constant" `Quick test_exists_without_universals_uses_skolem_constant;
         test_case "existential under forall" `Quick test_exists_under_forall_uses_skolem_function;
         test_case "nested skolem dependencies" `Quick test_nested_existential_depends_on_all_visible_universals;
         test_case "same existential block" `Quick test_same_existential_block_gets_independent_skolem_terms;
         test_case "shadowed variables" `Quick test_standardization_handles_shadowed_variables_before_skolemization;
         test_case "formula skolem collision" `Quick test_skolem_names_avoid_existing_function_symbols_in_formula;
         test_case "input-wide skolem collision" `Quick test_skolem_names_avoid_existing_function_symbols_across_inputs;
         test_case "one-way definition" `Quick test_one_way_definition;
         test_case "one-way trace metadata" `Quick test_one_way_trace_metadata;
         test_case "guarded one-way definition" `Quick test_guarded_one_way_definition;
         test_case "dmt definition expansion" `Quick test_dmt_definition_expansion;
         test_case "dmt conjoined definitions" `Quick test_dmt_collects_conjoined_definitions;
       ]);
    ]
