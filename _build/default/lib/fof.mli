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
  | Input_cnf of { name : string; role : string; clause : clause }
  | Input_fof of { name : string; role : string; formula : formula }
  | Input_include of include_spec

val vars_of_term : Types.StringSet.t -> term -> Types.StringSet.t
val vars_of_formula : Types.StringSet.t -> formula -> Types.StringSet.t
val subst_term : (string * term) list -> term -> term
val subst_formula : (string * term) list -> formula -> formula
