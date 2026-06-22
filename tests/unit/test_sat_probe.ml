open Alcotest
open Prover_lib
open Types

let const name = Fun (name, [])
let atom pred args = { pred; args }
let pos pred args = Pos (atom pred args)
let neg pred args = Neg (atom pred args)

let check_result name expected actual =
  let pp ppf = function
    | Sat_probe.Satisfiable -> Format.fprintf ppf "Satisfiable"
    | Sat_probe.Unsat -> Format.fprintf ppf "Unsat"
    | Sat_probe.Not_applicable reason ->
        Format.fprintf ppf "Not_applicable(%s)" reason
    | Sat_probe.Too_large reason ->
        Format.fprintf ppf "Too_large(%s)" reason
  in
  check (testable pp ( = )) name expected actual

let test_ground_sat () =
  check_result
    "ground sat"
    Sat_probe.Satisfiable
    (Sat_probe.run [[ pos "p" [ const "a" ] ]; [ neg "q" [ const "b" ] ]])

let test_epr_unsat () =
  check_result
    "epr unsat"
    Sat_probe.Unsat
    (Sat_probe.run
       [
         [ pos "p" [ Var "X" ] ];
         [ neg "p" [ const "a" ] ];
       ])

let test_reject_function_symbols () =
  match
    Sat_probe.run [[ pos "p" [ Fun ("f", [ const "a" ]) ] ]]
  with
  | Sat_probe.Not_applicable _ -> ()
  | r -> check_result "non-constant functions rejected" (Sat_probe.Not_applicable "") r

let test_reject_equality () =
  match Sat_probe.run [[ pos "=" [ const "a"; const "b" ] ]] with
  | Sat_probe.Not_applicable _ -> ()
  | r -> check_result "equality rejected" (Sat_probe.Not_applicable "") r

let () =
  run "Sat probe"
    [
      ("safe fragment",
       [
         test_case "ground sat" `Quick test_ground_sat;
         test_case "epr unsat" `Quick test_epr_unsat;
         test_case "reject function symbols" `Quick test_reject_function_symbols;
         test_case "reject equality" `Quick test_reject_equality;
       ]);
    ]
