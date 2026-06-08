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

exception Timeout_hit

val reset_fresh_state : unit -> unit

val clausify_formula : ?check_timeout:(unit -> unit) -> formula -> clause list
val clauses_of_input : ?check_timeout:(unit -> unit) -> annotated_input list -> clause list
val clauses_of_input_with_report :
  ?check_timeout:(unit -> unit) -> annotated_input list -> clause list * clausification_report
val partition_input_clauses :
  ?check_timeout:(unit -> unit) -> annotated_input list -> partitioned_clauses
