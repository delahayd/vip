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

let test_include_as_symbol () =
  let s = "fof(f1, axiom, means(include, X) & concept2words_2(X, include))." in
  let xs = Tptp_frontend.parse_string s in
  check int "one item" 1 (List.length xs)

let test_numeric_constant_as_uninterpreted_symbol () =
  let xs = Tptp_frontend.parse_string "fof(f1,axiom,p(0))." in
  check int "one item" 1 (List.length xs)

let expect_input_error input () =
  match Tptp_frontend.parse_string input with
  | exception Tptp_frontend.Error _ -> ()
  | _ -> fail "expected unsupported TPTP input to be rejected"

let () =
  run "parser"
    [
      ("tptp",
       [
         test_case "parse cnf" `Quick test_parse_cnf;
         test_case "parse fof" `Quick test_parse_fof;
         test_case "include as symbol" `Quick test_include_as_symbol;
         test_case "numeric constant" `Quick
           test_numeric_constant_as_uninterpreted_symbol;
         test_case "reject unsupported defined predicate" `Quick
           (expect_input_error "fof(f1,axiom,$distinct(a,b)).");
       ]);
    ]
