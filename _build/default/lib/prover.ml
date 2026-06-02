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
      Printf.printf "%% wall clock seconds      : %.6f\n" s.wall_clock_s
