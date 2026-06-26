open Types

type formula =
  | FTrue
  | FFalse
  | Atom of atom
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Imp of formula * formula
  | RevImp of formula * formula
  | Iff of formula * formula
  | Xor of formula * formula
  | Forall of string list * formula
  | Exists of string list * formula

type include_spec = {
  include_file : string;
  include_only : string list option;
}

type annotated_input =
  | Input_cnf of {
      name : string;
      role : string;
      clause : clause;
      source_file : string option;
    }
  | Input_fof of {
      name : string;
      role : string;
      formula : formula;
      source_file : string option;
    }
  | Input_include of include_spec

let rec vars_of_term acc = function
  | Var v -> StringSet.add v acc
  | Fun (_, ts) -> List.fold_left vars_of_term acc ts

let vars_of_atom acc a =
  List.fold_left vars_of_term acc a.args

let rec vars_of_formula acc = function
  | FTrue | FFalse -> acc
  | Atom a -> vars_of_atom acc a
  | Not f -> vars_of_formula acc f
  | And (a, b)
  | Or (a, b)
  | Imp (a, b)
  | RevImp (a, b)
  | Iff (a, b)
  | Xor (a, b) -> vars_of_formula (vars_of_formula acc a) b
  | Forall (_, f)
  | Exists (_, f) -> vars_of_formula acc f

let subst_lookup subs v =
  match List.find_opt (fun (x, _) -> x = v) subs with
  | None -> None
  | Some (_, t) -> Some t

let rec subst_term subs = function
  | Var v ->
      begin match subst_lookup subs v with
      | None -> Var v
      | Some t -> t
      end
  | Fun (f, args) -> Fun (f, List.map (subst_term subs) args)

let subst_atom subs a =
  { a with args = List.map (subst_term subs) a.args }

let rec subst_formula subs = function
  | FTrue -> FTrue
  | FFalse -> FFalse
  | Atom a -> Atom (subst_atom subs a)
  | Not f -> Not (subst_formula subs f)
  | And (a, b) -> And (subst_formula subs a, subst_formula subs b)
  | Or (a, b) -> Or (subst_formula subs a, subst_formula subs b)
  | Imp (a, b) -> Imp (subst_formula subs a, subst_formula subs b)
  | RevImp (a, b) -> RevImp (subst_formula subs a, subst_formula subs b)
  | Iff (a, b) -> Iff (subst_formula subs a, subst_formula subs b)
  | Xor (a, b) -> Xor (subst_formula subs a, subst_formula subs b)
  | Forall (vs, f) ->
      let subs' = List.filter (fun (x, _) -> not (List.mem x vs)) subs in
      Forall (vs, subst_formula subs' f)
  | Exists (vs, f) ->
      let subs' = List.filter (fun (x, _) -> not (List.mem x vs)) subs in
      Exists (vs, subst_formula subs' f)
