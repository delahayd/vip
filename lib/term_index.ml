open Types

type orientation_mode =
  | Oriented_only
  | Both_if_unorientable

type term_entry = {
  clause_id : int;
  lit_index : int;
  arg_index : int;
  path : int list;
  subterm : term;
}

type equality_entry = {
  clause_id : int;
  lit_index : int;
  lhs : term;
  rhs : term;
}

type root_key =
  | RootAny
  | RootFun of string * int

type t = {
  terms : (root_key, term_entry list ref) Hashtbl.t;
  equalities : (root_key, equality_entry list ref) Hashtbl.t;
  mutable all_terms : term_entry list;
  mutable all_equalities : equality_entry list;
  clause_terms : (int, term_entry list) Hashtbl.t;
  clause_equalities : (int, equality_entry list) Hashtbl.t;
}

let create () =
  {
    terms = Hashtbl.create 251;
    equalities = Hashtbl.create 251;
    all_terms = [];
    all_equalities = [];
    clause_terms = Hashtbl.create 4099;
    clause_equalities = Hashtbl.create 4099;
  }

let bucket tbl key =
  match Hashtbl.find_opt tbl key with
  | Some r -> r
  | None ->
      let r = ref [] in
      Hashtbl.add tbl key r;
      r

let root_key_of_term = function
  | Var _ -> RootAny
  | Fun (f, args) -> RootFun (f, List.length args)

let atom_of_literal = function
  | Pos a | Neg a -> a

let positive_equality = function
  | Pos { pred = "="; args = [ l; r ] } -> Some (l, r)
  | _ -> None

let non_variable_subterms t =

  let rec aux path acc = function
    | Var _ -> acc
    | Fun (_, args) as u ->
        let acc = (path, u) :: acc in
        List.mapi (fun i x -> (i, x)) args
        |> List.fold_left
             (fun acc (i, child) -> aux (path @ [ i ]) acc child)
             acc
  in
  aux [] [] t

let add_term_entry idx e =
  idx.all_terms <- e :: idx.all_terms;
  let key = root_key_of_term e.subterm in
  let b = bucket idx.terms key in
  b := e :: !b

let add_equality_entry idx e =
  idx.all_equalities <- e :: idx.all_equalities;
  let key = root_key_of_term e.lhs in
  let b = bucket idx.equalities key in
  b := e :: !b

let equality_orientations orientation_mode l r =
  match Ordering.orient_equation l r with
  | Some (lhs, rhs) -> [ lhs, rhs ]
  | None ->
      begin
        match orientation_mode with
        | Oriented_only -> []
        | Both_if_unorientable -> [ l, r; r, l ]
      end

let add_clause idx ~clause_id ~orientation_mode clause =
  let indexed_terms = ref [] in
  let indexed_equalities = ref [] in
  List.iteri
    (fun lit_index lit ->
      let a = atom_of_literal lit in

      List.iteri
        (fun arg_index arg ->
          List.iter
            (fun (path, subterm) ->
              let entry = { clause_id; lit_index; arg_index; path; subterm } in
              indexed_terms := entry :: !indexed_terms;
              add_term_entry idx entry)
            (non_variable_subterms arg))
        a.args;

      match positive_equality lit with
      | None -> ()
      | Some (l, r) ->
          List.iter
            (fun (lhs, rhs) ->
              let entry = { clause_id; lit_index; lhs; rhs } in
              indexed_equalities := entry :: !indexed_equalities;
              add_equality_entry idx entry)
            (equality_orientations orientation_mode l r))
    clause;
  Hashtbl.replace idx.clause_terms clause_id !indexed_terms;
  Hashtbl.replace idx.clause_equalities clause_id !indexed_equalities

let remove_from_bucket tbl key predicate =
  match Hashtbl.find_opt tbl key with
  | None -> ()
  | Some bucket ->
      bucket := List.filter predicate !bucket;
      if !bucket = [] then
        Hashtbl.remove tbl key

let remove_clause idx ~clause_id =
  idx.all_terms <-
    List.filter (fun (e : term_entry) -> e.clause_id <> clause_id) idx.all_terms;
  idx.all_equalities <-
    List.filter
      (fun (e : equality_entry) -> e.clause_id <> clause_id)
      idx.all_equalities;
  begin
    match Hashtbl.find_opt idx.clause_terms clause_id with
    | None -> ()
    | Some entries ->
        List.iter
          (fun (entry : term_entry) ->
            remove_from_bucket
              idx.terms
              (root_key_of_term entry.subterm)
              (fun (e : term_entry) -> e.clause_id <> clause_id))
          entries;
        Hashtbl.remove idx.clause_terms clause_id
  end;
  begin
    match Hashtbl.find_opt idx.clause_equalities clause_id with
    | None -> ()
    | Some entries ->
        List.iter
          (fun (entry : equality_entry) ->
            remove_from_bucket
              idx.equalities
              (root_key_of_term entry.lhs)
              (fun (e : equality_entry) -> e.clause_id <> clause_id))
          entries;
        Hashtbl.remove idx.clause_equalities clause_id
  end

let find_bucket_or_all tbl all term =
  match term with
  | Var _ -> all
  | Fun (f, args) ->
      begin
        match Hashtbl.find_opt tbl (RootFun (f, List.length args)) with
        | None -> []
        | Some r -> !r
      end

let find_terms_unifiable_with idx term =
  find_bucket_or_all idx.terms idx.all_terms term

let find_equalities_unifiable_with idx term =
  find_bucket_or_all idx.equalities idx.all_equalities term
