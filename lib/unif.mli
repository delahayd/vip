open Types

exception Not_unifiable

val unify_terms : term -> term -> substitution -> substitution
val unify_atoms : atom -> atom -> substitution -> substitution
