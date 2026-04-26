open Alcotest
open Prover_lib

let test_parse_cnf () =
  let s = "cnf(c1, axiom, p(a) | ~q(X))." in
  let xs = Tptp_frontend.parse_string s in
  check int "one item" 1 (List.length xs)

let test_parse_fof () =
  let s = "fof(f1, axiom, ! [X] : (p(X) => q(X)))." in
  let xs = Tptp_frontend.parse_string s in
  check int "one item" 1 (List.length xs)

let () =
  run "parser"
    [
      ("tptp",
       [
         test_case "parse cnf" `Quick test_parse_cnf;
         test_case "parse fof" `Quick test_parse_fof;
       ]);
    ]
