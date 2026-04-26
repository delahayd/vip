open Types

val empty_subst : substitution
val apply_subst_term : substitution -> term -> term
val apply_subst_atom : substitution -> atom -> atom
val apply_subst_literal : substitution -> literal -> literal
val apply_subst_clause : substitution -> clause -> clause
val compose_subst : substitution -> substitution -> substitution
val occurs : string -> term -> bool
