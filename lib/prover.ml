open Types
open Tptp_frontend
open Clausify
open Resolution

type szs_status =
  | Theorem
  | Unsatisfiable
  | Satisfiable
  | CounterSatisfiable
  | GaveUp
  | Timeout
  | ResourceOut
  | InputError

type config = {
  time_limit_s : float option;
  max_generated_clauses : int option;
  print_derivation : bool;
  inference_mode : Resolution.inference_mode;
}

type problem_info = {
  file : string option;
  clause_count : int;
  generated_clause_count : int;
}

type outcome = {
  status : szs_status;
  info : problem_info;
  derivation : Resolution.derived list;
  empty_clause : Resolution.derived option;
  resolution_stats : Resolution.stats option;
}

let default_config = {
  time_limit_s = None;
  max_generated_clauses = None;
  print_derivation = false;
  inference_mode = Resolution.Ordered_with_fallback;
}

let string_of_szs_status = function
  | Theorem -> "Theorem"
  | Unsatisfiable -> "Unsatisfiable"
  | Satisfiable -> "Satisfiable"
  | CounterSatisfiable -> "CounterSatisfiable"
  | GaveUp -> "GaveUp"
  | Timeout -> "Timeout"
  | ResourceOut -> "ResourceOut"
  | InputError -> "InputError"

let clauses_of_file filename =
  let parsed = load_problem filename in
  let clauses, _report = clauses_of_input_with_report parsed.inputs in
  clauses

let infer_status_from_stop_reason = function
  | Refutation_found _ -> Unsatisfiable
  | Saturation -> GaveUp
  | Time_limit -> Timeout
  | Clause_limit -> ResourceOut

let run_file ?(config = default_config) filename =
  let parsed = load_problem filename in
  let part = partition_input_clauses parsed.inputs in

  let timeout_outcome () =
    {
      status = Timeout;
      info = {
        file = Some filename;
        clause_count = List.length part.axioms + List.length part.support;
        generated_clause_count = 0;
      };
      derivation = [];
      empty_clause = None;
      resolution_stats = None;
    }
  in

  try
    let limits = {
      Resolution.time_limit_s = config.time_limit_s;
      max_generated_clauses = config.max_generated_clauses;
    } in

    let res =
      match part.support with
      | [] ->
          Resolution.run_resolution_sos
            ~limits
            ~mode:config.inference_mode
            ~axioms:[]
            ~support:part.axioms
            ()
      | _ ->
          Resolution.run_resolution_sos
            ~limits
            ~mode:config.inference_mode
            ~axioms:part.axioms
            ~support:part.support
            ()
    in

    let empty_clause =
      match res.stop_reason with
      | Refutation_found d -> Some d
      | Saturation | Time_limit | Clause_limit -> None
    in

    {
      status = infer_status_from_stop_reason res.stop_reason;
      info = {
        file = Some filename;
        clause_count = List.length part.axioms + List.length part.support;
        generated_clause_count = res.stats.generated_clauses;
      };
      derivation = res.derivation;
      empty_clause;
      resolution_stats = Some res.stats;
    }
  with
  | Resolution.Timeout_hit ->
      ignore (Unix.alarm 0);
      timeout_outcome ()

let print_szs outcome =
  let name =
    match outcome.info.file with
    | Some f -> f
    | None -> "<stdin>"
  in

  Printf.printf
    "%% SZS status %s for %s\n"
    (string_of_szs_status outcome.status)
    name;

  match outcome.resolution_stats with
  | None -> ()
  | Some s ->
      Printf.printf "%% clauses input           : %d\n" outcome.info.clause_count;
      Printf.printf "%% clauses generated       : %d\n" s.generated_clauses;
      Printf.printf "%% clauses processed       : %d\n" s.processed_clauses;
      Printf.printf "%% resolution inferences   : %d\n" s.resolution_inferences;
      Printf.printf "%% factoring inferences    : %d\n" s.factoring_inferences;
      Printf.printf "%% equality resolution     : %d\n" s.equality_resolution_inferences;
      Printf.printf "%% equality factoring      : %d\n" s.equality_factoring_inferences;
      Printf.printf "%% superposition infer.    : %d\n" s.superposition_inferences;
      Printf.printf "%% demodulation rewrites   : %d\n" s.demodulation_rewrites;
      Printf.printf "%% subsumption tests       : %d\n" s.subsumption_tests;
      Printf.printf "%% subsumption rejections  : %d\n" s.subsumption_rejections;
      Printf.printf "%% avatar splitting        : %s\n" (if s.avatar_enabled then "enabled" else "disabled");
      Printf.printf "%% avatar keep original    : %s\n" (if s.avatar_keep_original then "yes" else "no");
      Printf.printf "%% avatar min split lits   : %d\n" s.avatar_min_split_literals;
      Printf.printf "%% avatar max split vars   : %d\n" s.avatar_max_split_vars;
      Printf.printf "%% avatar split vars used  : %d\n" s.avatar_split_vars_used;
      Printf.printf "%% avatar split attempts   : %d\n" s.avatar_split_attempts;
      Printf.printf "%% avatar successful split : %d\n" s.avatar_successful_splits;
      Printf.printf "%% avatar splits           : %d\n" s.avatar_splits;
      Printf.printf "%% avatar split components : %d\n" s.avatar_split_components;
      Printf.printf "%% avatar reject disabled  : %d\n" s.avatar_split_rejected_disabled;
      Printf.printf "%% avatar reject short     : %d\n" s.avatar_split_rejected_short;
      Printf.printf "%% avatar reject equality  : %d\n" s.avatar_split_rejected_equality;
      Printf.printf "%% avatar reject nonground : %d\n" s.avatar_split_rejected_nonground;
      Printf.printf "%% avatar reject trivial   : %d\n" s.avatar_split_rejected_trivial;
      Printf.printf "%% avatar reject quota     : %d\n" s.avatar_split_rejected_quota;
      Printf.printf "%% avatar ctx clauses gen. : %d\n" s.avatar_context_clauses_generated;
      Printf.printf "%% avatar ctx empty conf.  : %d\n" s.avatar_contextual_empty_conflicts;
      Printf.printf "%% avatar learned conf.    : %d\n" s.avatar_learned_conflicts;
      Printf.printf "%% avatar model blocks     : %d\n" s.avatar_model_blocks;
      Printf.printf "%% avatar SAT clauses      : %d\n" s.avatar_sat_clauses_added;
      Printf.printf "%% avatar SAT solves       : %d\n" s.avatar_sat_solves;
      Printf.printf "%% avatar SAT conflicts    : %d\n" s.avatar_sat_conflicts;
      Printf.printf "%% avatar ctx SAT tests    : %d\n" s.avatar_context_sat_tests;
      Printf.printf "%% avatar ctx SAT failures : %d\n" s.avatar_context_sat_failures;
      Printf.printf "%% avatar filtered infer.  : %d\n" s.avatar_filtered_inferences;
      Printf.printf "%% wall clock seconds      : %.6f\n" s.wall_clock_s
