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
      }
  in
  let baseline, _ = Clausify.clauses_of_input_with_report [ input ] in
  let one_way =
    with_env "IP_ONE_WAY_DEFINITIONS" (Some "1") (fun () ->
      fst (Clausify.clauses_of_input_with_report [ input ]))
  in
  check int "baseline clauses" 2 (List.length baseline);
  check int "one-way clauses" 1 (List.length one_way)

let () =
  run "clausify"
    [
      ("fof",
       [
         test_case "simple implication" `Quick test_simple_clausification;
         test_case "one-way definition" `Quick test_one_way_definition;
       ]);
    ]
