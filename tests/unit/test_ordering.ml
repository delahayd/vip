open Alcotest
open Prover_lib
open Types
open Ordering

let var x = Var x
let const c = Fun (c, [])
let fun_ f args = Fun (f, args)

let atom p args = { pred = p; args = args }
let pos a = Pos a
let neg a = Neg a

let test_kbo_1 () =
  (* f(x,y) > x *)
  let t1 = fun_ "f" [var "X"; var "Y"] in
  let t2 = var "X" in
  check bool "f(x,y) > x" true (strictly_greater_term_kbo t1 t2)

let test_kbo_2 () =
  (* x !> f(x,y) *)
  let t1 = var "X" in
  let t2 = fun_ "f" [var "X"; var "Y"] in
  check bool "x !> f(x,y)" false (strictly_greater_term_kbo t1 t2)

let test_kbo_3 () =
  (* f(a) > a *)
  let t1 = fun_ "f" [const "a"] in
  let t2 = const "a" in
  check bool "f(a) > a" true (strictly_greater_term_kbo t1 t2)

let test_maximal_literal () =
  (* P(f(X, Y)) v Q(X) -> P is maximal *)
  let l1 = pos (atom "p" [fun_ "f" [var "X"; var "Y"]]) in
  let l2 = pos (atom "q" [var "X"]) in
  let c = [ l1; l2 ] in
  let max_indices = maximal_literal_indices c in
  check bool "index 0 is maximal" true (List.mem 0 max_indices);
  check bool "index 1 is NOT maximal" false (List.mem 1 max_indices)

let () =
  run "Term Ordering (KBO)"
    [
      ("kbo_terms",
       [
         test_case "f(x,y) > x" `Quick test_kbo_1;
         test_case "x !> f(x,y)" `Quick test_kbo_2;
         test_case "f(a) > a" `Quick test_kbo_3;
       ]);
      ("literals",
       [
         test_case "maximal literal index" `Quick test_maximal_literal;
       ])
    ]
