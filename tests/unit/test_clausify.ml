open Alcotest
open Prover_lib
open Types
open Fof

let test_simple_clausification () =
  let f =
    Forall
      ( [ "X" ],
        Imp
          ( Atom { pred = "p"; args = [ Var "X" ] },
            Atom { pred = "q"; args = [ Var "X" ] } ) )
  in
  let clauses = Clausify.clausify_formula f in
  check bool "has clauses" true (List.length clauses > 0)

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
  let input =
    Input_fof
      {
        name = "def_p";
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Iff
                ( Atom { pred = "p"; args = [ Var "X" ] },
                  Atom { pred = "q"; args = [ Var "X" ] } ) );
        source_file = None;
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ input ] in
  let one_way =
    with_env "IP_ONE_WAY_DEFINITIONS" (Some "1") (fun () ->
      fst (Clausify.clauses_of_input_with_report [ input ]))
  in
  check int "baseline clauses" 2 (List.length baseline);
  check int "one-way clauses" 1 (List.length one_way)

let test_guarded_one_way_definition () =
  let input =
    Input_fof
      {
        name = "guarded_def_p";
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Imp
                ( Atom { pred = "r"; args = [ Var "X" ] },
                  Iff
                    ( Atom { pred = "p"; args = [ Var "X" ] },
                      Atom { pred = "q"; args = [ Var "X" ] } ) ) );
        source_file = None;
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ input ] in
  let one_way =
    with_env "IP_ONE_WAY_DEFINITIONS" (Some "1") (fun () ->
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
  let def =
    Input_fof
      {
        name = "def_p";
        role = "axiom";
        formula =
          Forall
            ( [ "X" ],
              Iff
                ( Atom { pred = "p"; args = [ Var "X" ] },
                  Atom { pred = "q"; args = [ Var "X" ] } ) );
        source_file = None;
      }
  in
  let use =
    Input_fof
      {
        name = "use_p";
        role = "axiom";
        formula = Atom { pred = "p"; args = [ Fun ("a", []) ] };
        source_file = None;
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ def; use ] in
  let expanded =
    with_envs
      [ ("IP_DMT_EXPAND_DEFINITIONS", "1"); ("IP_ONE_WAY_DEFINITIONS", "0") ]
      (fun () -> fst (Clausify.clauses_of_input_with_report [ def; use ]))
  in
  check int "baseline keeps definition" 3 (List.length baseline);
  check int "expanded removes definition" 1 (List.length expanded);
  check
    (list string)
    "rewritten clause"
    [ "q(a)" ]
    (List.map Pretty.string_of_clause expanded)

let () =
  run "clausify"
    [
      ("fof",
       [
         test_case "simple implication" `Quick test_simple_clausification;
         test_case "one-way definition" `Quick test_one_way_definition;
         test_case "guarded one-way definition" `Quick test_guarded_one_way_definition;
         test_case "dmt definition expansion" `Quick test_dmt_definition_expansion;
       ]);
    ]
