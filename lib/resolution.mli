open Types

exception Timeout_hit
exception Rewrite_limit_hit

type inference_mode =
  | Unrestricted
  | Ordered
  | Ordered_with_fallback

type split_var = int

type prop_lit =
  | PPos of split_var
  | PNeg of split_var

type context = prop_lit list

type derived = {
  id : int;
  parents : int list;
  rule : string;
  clause_d : clause;
  context : context;
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
  equality_resolution_inferences : int;
  equality_factoring_inferences : int;
  superposition_inferences : int;
  demodulation_rewrites : int;
  subsumption_tests : int;
  subsumption_rejections : int;

  avatar_enabled : bool;
  avatar_keep_original : bool;
  avatar_min_split_literals : int;
  avatar_max_split_vars : int;
  avatar_split_vars_used : int;
  avatar_split_attempts : int;
  avatar_successful_splits : int;
  avatar_splits : int;
  avatar_split_components : int;
  avatar_split_rejected_disabled : int;
  avatar_split_rejected_short : int;
  avatar_split_rejected_equality : int;
  avatar_split_rejected_nonground : int;
  avatar_split_rejected_trivial : int;
  avatar_split_rejected_quota : int;
  avatar_context_clauses_generated : int;
  avatar_contextual_empty_conflicts : int;
  avatar_learned_conflicts : int;
  avatar_model_blocks : int;
  avatar_sat_clauses_added : int;
  avatar_sat_solves : int;
  avatar_sat_conflicts : int;
  avatar_context_sat_tests : int;
  avatar_context_sat_failures : int;
  avatar_filtered_inferences : int;

  wall_clock_s : float;
}

type run_result = {
  stop_reason : stop_reason;
  derivation : derived list;
  stats : stats;
}

val default_limits : limits

val string_of_prop_lit : prop_lit -> string
val string_of_context : context -> string

val normalize_context : context -> context
val context_subsumes : context -> context -> bool

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
