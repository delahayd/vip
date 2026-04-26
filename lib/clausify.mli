open Types
open Fof

type clausification_report = {
  input_count : int;
  produced_clause_count : int;
}

type partitioned_clauses = {
  axioms : clause list;
  support : clause list;
  report : clausification_report;
}

val clausify_formula : formula -> clause list
val clauses_of_input : annotated_input list -> clause list
val clauses_of_input_with_report : annotated_input list -> clause list * clausification_report
val partition_input_clauses : annotated_input list -> partitioned_clauses
