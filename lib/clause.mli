open Types

val compare_term : term -> term -> int
val compare_atom : atom -> atom -> int
val compare_literal : literal -> literal -> int
val normalize_clause : clause -> clause
val canonicalize_clause_vars : clause -> clause
val vars_of_clause : clause -> Types.StringSet.t
val rename_clause_apart : clause -> clause
val complementary : literal -> literal -> bool
val clause_is_tautology : clause -> bool
val simplify_clause : clause -> clause option
val subsumes : clause -> clause -> bool
val subsumption_resolution : clause -> clause -> clause option

val literal_weight : literal -> int
val maximal_literal_indices : clause -> int list
