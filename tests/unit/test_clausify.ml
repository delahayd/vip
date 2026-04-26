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

let () =
  run "clausify"
    [
      ("fof",
       [
         test_case "simple implication" `Quick test_simple_clausification;
       ]);
    ]
