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

type t

val create : unit -> t

val add_clause :
  t ->
  clause_id:int ->
  orientation_mode:orientation_mode ->
  clause ->
  unit

val find_terms_unifiable_with :
  t ->
  term ->
  term_entry list

val find_equalities_unifiable_with :
  t ->
  term ->
  equality_entry list
