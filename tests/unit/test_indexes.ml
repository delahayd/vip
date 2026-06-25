open Alcotest
open Prover_lib
open Types

let const c = Fun (c, [])
let fun_ f args = Fun (f, args)
let atom pred args = { pred; args }
let pos a = Pos a
let neg a = Neg a
let eq l r = pos (atom "=" [ l; r ])

let clause_id_set ids =
  ids |> List.sort_uniq Int.compare

let term_entry_id (e : Term_index.term_entry) =
  e.Term_index.clause_id

let equality_entry_id (e : Term_index.equality_entry) =
  e.Term_index.clause_id

let test_discrimination_index_remove_clause () =
  let idx = Discrimination_index.create () in
  let c1 = [ pos (atom "p" [ const "a" ]) ] in
  let c2 = [ neg (atom "p" [ const "a" ]) ] in
  Discrimination_index.add_clause idx ~clause_id:1 c1;
  Discrimination_index.add_clause idx ~clause_id:2 c2;
  let before =
    Discrimination_index.find_complementary idx (List.hd c1)
    |> List.map (fun e -> e.Discrimination_index.clause_id)
    |> clause_id_set
  in
  check (list int) "before removal" [ 2 ] before;
  Discrimination_index.remove_clause idx ~clause_id:2;
  let after =
    Discrimination_index.find_complementary idx (List.hd c1)
    |> List.map (fun e -> e.Discrimination_index.clause_id)
    |> clause_id_set
  in
  check (list int) "after removal" [] after

let test_term_index_remove_clause () =
  let idx = Term_index.create () in
  let fa = fun_ "f" [ const "a" ] in
  let ga = fun_ "g" [ const "a" ] in
  let c1 = [ pos (atom "p" [ fa ]); eq fa (const "b") ] in
  let c2 = [ pos (atom "p" [ ga ]); eq ga (const "c") ] in
  Term_index.add_clause
    idx
    ~clause_id:1
    ~orientation_mode:Term_index.Both_if_unorientable
    c1;
  Term_index.add_clause
    idx
    ~clause_id:2
    ~orientation_mode:Term_index.Both_if_unorientable
    c2;
  Term_index.remove_clause idx ~clause_id:1;
  let f_term_ids =
    Term_index.find_terms_unifiable_with idx fa
    |> List.map term_entry_id
    |> clause_id_set
  in
  let f_eq_ids =
    Term_index.find_equalities_unifiable_with idx fa
    |> List.map equality_entry_id
    |> clause_id_set
  in
  let g_term_ids =
    Term_index.find_terms_unifiable_with idx ga
    |> List.map term_entry_id
    |> clause_id_set
  in
  check (list int) "removed term entries" [] f_term_ids;
  check (list int) "removed equality entries" [] f_eq_ids;
  check (list int) "kept other term entries" [ 2 ] g_term_ids

let () =
  run
    "Indexes"
    [
      ( "removal",
        [
          test_case
            "discrimination index removes clause entries"
            `Quick
            test_discrimination_index_remove_clause;
          test_case
            "term index removes clause entries"
            `Quick
            test_term_index_remove_clause;
        ] );
    ]
