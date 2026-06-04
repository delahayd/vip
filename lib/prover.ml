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
  tptp_dir : string option;
  use_sos : bool;
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
  tptp_dir = None;
  use_sos = true;
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
  let load_config =
    match config.tptp_dir with
    | None -> default_load_config
    | Some d -> { include_paths = [ d ]; use_tptp_env = true }
  in
  let parsed = load_problem ~config:load_config filename in
  let part = partition_input_clauses parsed.inputs in

  let axioms, support =
    if config.use_sos then (part.axioms, part.support)
    else ([], part.axioms @ part.support)
  in

  let timeout_outcome () =
    {
      status = Timeout;
      info = {
        file = Some filename;
        clause_count = List.length axioms + List.length support;
        generated_clause_count = 0;
      };
      derivation = [];
      empty_clause = None;
      resolution_stats = None;
    }
  in

  try
    (* Portfolio Strategy: Stage 1 "Flash Mode" *)
    if config.print_derivation then Printf.printf "%% Stage 1: Flash Mode (2s)\n%!";
    let flash_timeout = 2.0 in
    let total_timeout =
      match config.time_limit_s with
      | Some t -> t
      | None -> 6.0 (* Default to 6s as requested *)
    in

    let flash_limits = {
      Resolution.time_limit_s = Some (min flash_timeout total_timeout);
      max_generated_clauses = config.max_generated_clauses;
    } in

    let flash_res =
      try
        Resolution.run_resolution_sos
          ~limits:flash_limits
          ~expensive_simplifications:false
          ~mode:config.inference_mode
          ~axioms
          ~support
          ()

      with Resolution.Timeout_hit ->
        (* Stage 1 timed out, which is expected for hard problems *)
        {
          Resolution.stop_reason = Time_limit;
          derivation = [];
          stats = {
            generated_clauses = 0;
            processed_clauses = 0;
            resolution_inferences = 0;
            factoring_inferences = 0;
            equality_resolution_inferences = 0;
            equality_factoring_inferences = 0;
            superposition_inferences = 0;
            demodulation_rewrites = 0;
            subsumption_tests = 0;
            subsumption_rejections = 0;
            wall_clock_s = flash_timeout;
          };
        }
    in

    let res =
      match flash_res.stop_reason with
      | Refutation_found _ -> flash_res
      | Saturation -> flash_res (* Exhausted search space *)
      | Time_limit | Clause_limit ->
          (* Stage 2 "Deep Mode" for the remaining time *)
          let wall_s = flash_res.stats.wall_clock_s in
          let remaining_time = total_timeout -. wall_s in
          if remaining_time <= 0.1 then flash_res
          else
            let deep_limits = {
              Resolution.time_limit_s = Some remaining_time;
              max_generated_clauses = config.max_generated_clauses;
            } in
            if config.print_derivation then
              Printf.printf "%% Stage 1 failed. Entering Stage 2: Deep Mode (%.2fs)\n%!" remaining_time;
            Resolution.run_resolution_sos
              ~limits:deep_limits
              ~expensive_simplifications:true
              ~mode:config.inference_mode
              ~axioms
              ~support
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
      Printf.printf "%% wall clock seconds      : %.6f\n" s.wall_clock_s
