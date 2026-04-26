open Types

type inference_mode =
  | Unrestricted
  | Ordered
  | Ordered_with_fallback

type derived = {
  id : int;
  parents : int list;
  rule : string;
  clause_d : clause;
}

type limits = {
  time_limit_s : float option;
  max_generated_clauses : int option;
}

type stop_reason =
  | Refutation_found of derived
  | Saturation
  | Time_limit
  | Clause_limit

type stats = {
  generated_clauses : int;
  processed_clauses : int;
  resolution_inferences : int;
  factoring_inferences : int;
  subsumption_tests : int;
  subsumption_rejections : int;
  wall_clock_s : float;
}

type run_result = {
  stop_reason : stop_reason;
  derivation : derived list;
  stats : stats;
}

val default_limits : limits

val run_resolution_sos :
  ?limits:limits ->
  mode:inference_mode ->
  axioms:clause list ->
  support:clause list ->
  unit ->
  run_result

val run_resolution :
  ?limits:limits ->
  mode:inference_mode ->
  clause list ->
  run_result

val print_derivation : derived list -> unit
