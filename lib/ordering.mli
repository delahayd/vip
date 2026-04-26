open Types

val set_precedence : string list -> unit
val clear_precedence : unit -> unit

val term_weight : term -> int
val compare_term_kbo : term -> term -> int
val compare_atom : atom -> atom -> int
val compare_literal : literal -> literal -> int

val maximal_literal_indices : clause -> int list
