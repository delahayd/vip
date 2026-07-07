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

type clause_origin = {
  clause : clause;
  input_index : int;
  input_name : string;
  input_role : string;
  input_is_cnf : bool;
  one_way : bool;
  transformation_status : string;
}

type clausification_trace = {
  origins : clause_origin list;
}

type traced_partitioned_clauses = {
  partition : partitioned_clauses;
  trace : clausification_trace;
}

exception Timeout_hit

val reset_fresh_state : unit -> unit

val clausify_formula : ?check_timeout:(unit -> unit) -> formula -> clause list
val clauses_of_input : ?check_timeout:(unit -> unit) -> annotated_input list -> clause list
val clauses_of_input_with_report :
  ?check_timeout:(unit -> unit) -> annotated_input list -> clause list * clausification_report
val partition_input_clauses :
  ?check_timeout:(unit -> unit) -> annotated_input list -> partitioned_clauses
val partition_input_clauses_with_trace :
  ?check_timeout:(unit -> unit) -> annotated_input list -> traced_partitioned_clauses
