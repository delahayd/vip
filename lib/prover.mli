open Types

type szs_status =
  | Theorem
  | Unsatisfiable
  | Satisfiable
  | CounterSatisfiable
  | GaveUp
  | Timeout
  | ResourceOut
  | InputError

type portfolio_mode =
  | Legacy_then_modern
  | Modern_then_legacy
  | Legacy_only
  | Modern_only
  | Modern_compat_only
  | Feq_modern
  | Scheduled_portfolio
  | Experimental_casc
  | Casc_aggressive
  | Casc_240

type engine_kind =
  | Legacy_compat
  | Modern_compat_flash
  | Modern_deep
  | Modern_feq

type portfolio_stage = {
  stage_name : string;
  engine : engine_kind;
  time_limit_s : float;
}

type config = {
  time_limit_s : float option;
  max_generated_clauses : int option;
  print_derivation : bool;
  inference_mode : Resolution.inference_mode;
  tptp_dir : string option;
  use_sos : bool;
  portfolio_mode : portfolio_mode;
}

type problem_info = {
  file : string option;
  clause_count : int;
  generated_clause_count : int;
  profile : string;
  raw_clause_count : int;
  equality_literals : int;
  equality_literal_ratio : float;
  avg_literal_term_size : float;
  unit_ratio : float;
  negative_ratio : float;
  axiom_selection_enabled : bool;
}

type outcome = {
  status : szs_status;
  info : problem_info;
  derivation : Resolution.derived list;
  empty_clause : Resolution.derived option;
  resolution_stats : Resolution.stats option;
  tstp_prelude : string option;
}

val default_config : config
val clauses_of_file : string -> clause list
val run_file : ?config:config -> string -> outcome
val string_of_szs_status : szs_status -> string
val print_szs : ?verbose:bool -> outcome -> unit
