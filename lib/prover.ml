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

type portfolio_mode =
  | Legacy_then_modern
  | Legacy_only
  | Modern_only
  | Modern_compat_only

type engine_kind =
  | Legacy_compat
  | Modern_compat_flash
  | Modern_deep

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
  portfolio_mode = Legacy_then_modern;
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

let resolution_mode_of_legacy = function
  | Resolution.Unrestricted -> Legacy_resolution.Unrestricted
  | Resolution.Ordered -> Legacy_resolution.Ordered
  | Resolution.Ordered_with_fallback -> Legacy_resolution.Ordered_with_fallback

let derived_of_legacy (d : Legacy_resolution.derived) : Resolution.derived =
  {
    Resolution.id = d.id;
    parents = d.parents;
    rule = d.rule;
    clause_d = d.clause_d;
    is_active = false;
  }

let result_of_legacy (l_res : Legacy_resolution.run_result) : Resolution.run_result =
  {
    Resolution.stop_reason =
      (match l_res.stop_reason with
       | Legacy_resolution.Refutation_found d ->
           Resolution.Refutation_found (derived_of_legacy d)
       | Legacy_resolution.Saturation -> Resolution.Saturation
       | Legacy_resolution.Time_limit -> Resolution.Time_limit
       | Legacy_resolution.Clause_limit -> Resolution.Clause_limit);
    derivation = List.map derived_of_legacy l_res.derivation;
    stats =
      {
        Resolution.generated_clauses = l_res.stats.generated_clauses;
        processed_clauses = l_res.stats.processed_clauses;
        resolution_inferences = l_res.stats.resolution_inferences;
        factoring_inferences = l_res.stats.factoring_inferences;
        equality_resolution_inferences = l_res.stats.equality_resolution_inferences;
        equality_factoring_inferences = l_res.stats.equality_factoring_inferences;
        superposition_inferences = l_res.stats.superposition_inferences;
        demodulation_rewrites = l_res.stats.demodulation_rewrites;
        subsumption_tests = l_res.stats.subsumption_tests;
        subsumption_rejections = l_res.stats.subsumption_rejections;
        wall_clock_s = l_res.stats.wall_clock_s;
      };
  }

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
    let total_timeout =
      match config.time_limit_s with
      | Some t -> t
      | None -> 6.0
    in

    let timeout_result wall_clock_s =
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
          wall_clock_s;
        };
      }
    in

    let run_legacy_compat ~time_limit_s =
      let limits = {
        Legacy_resolution.time_limit_s = Some time_limit_s;
        max_generated_clauses = config.max_generated_clauses;
      } in
      try
        Legacy_resolution.run_resolution_sos
          ~limits
          ~mode:(resolution_mode_of_legacy config.inference_mode)
          ~axioms
          ~support
          ()
        |> result_of_legacy
      with Legacy_resolution.Timeout_hit ->
        timeout_result time_limit_s
    in

    let run_modern_resolution ~time_limit_s ~expensive_simplifications ~emulate_v1 =
      let limits = {
        Resolution.time_limit_s = Some time_limit_s;
        max_generated_clauses = config.max_generated_clauses;
      } in
      Resolution.run_resolution_sos
        ~limits
        ~expensive_simplifications
        ~emulate_v1
        ~mode:config.inference_mode
        ~axioms
        ~support
        ()
    in

    let run_modern_compat_flash ~time_limit_s =
      run_modern_resolution
        ~time_limit_s
        ~expensive_simplifications:false
        ~emulate_v1:true
    in

    let run_modern_deep ~time_limit_s =
      run_modern_resolution
        ~time_limit_s
        ~expensive_simplifications:true
        ~emulate_v1:false
    in

    let run_stage stage =
      if config.print_derivation then
        Printf.printf
          "%% Stage: %s (%.2fs)\n%!"
          stage.stage_name
          stage.time_limit_s;
      match stage.engine with
      | Legacy_compat -> run_legacy_compat ~time_limit_s:stage.time_limit_s
      | Modern_compat_flash ->
          run_modern_compat_flash ~time_limit_s:stage.time_limit_s
      | Modern_deep -> run_modern_deep ~time_limit_s:stage.time_limit_s
    in

    let run_legacy_then_modern () =
      let legacy_stage =
        {
          stage_name = "Legacy compatibility flash";
          engine = Legacy_compat;
          time_limit_s = min 3.0 total_timeout;
        }
      in
      let legacy_res = run_stage legacy_stage in
      match legacy_res.stop_reason with
      | Refutation_found _ -> legacy_res
      | Saturation | Time_limit | Clause_limit ->
          let remaining_time = total_timeout -. legacy_res.stats.wall_clock_s in
          if remaining_time <= 0.1 then legacy_res
          else
            run_stage
              {
                stage_name = "Modern deep search";
                engine = Modern_deep;
                time_limit_s = remaining_time;
              }
    in

    let res =
      match config.portfolio_mode with
      | Legacy_only ->
          run_stage
            {
              stage_name = "Legacy compatibility only";
              engine = Legacy_compat;
              time_limit_s = total_timeout;
            }
      | Modern_only ->
          run_stage
            {
              stage_name = "Modern deep only";
              engine = Modern_deep;
              time_limit_s = total_timeout;
            }
      | Modern_compat_only ->
          run_stage
            {
              stage_name = "Modern compat flash only";
              engine = Modern_compat_flash;
              time_limit_s = total_timeout;
            }
      | Legacy_then_modern ->
          run_legacy_then_modern ()
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
