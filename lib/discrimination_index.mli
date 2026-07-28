open Types

type entry = {
  clause_id : int;
  lit_index : int;
  literal : literal;
}

type t

val create : unit -> t
val add_clause : t -> clause_id:int -> clause -> unit
val find_complementary : t -> literal -> entry list
val find_complementary_candidates : t -> literal -> entry list
val find_same_sign_unifiable : t -> literal -> entry list
