open Prover_lib

type summary = {
  mutable solved : int;
  mutable timeout : int;
  mutable input_error : int;
  mutable other_failure : int;
  mutable total_time : float;
  mutable generated : int;
  mutable processed : int;
}

let usage () =
  prerr_endline
    "Usage: compare_modes (--dir DIR | --file-list FILE) --time-limit SECONDS [--root DIR] [--tptp DIR] [--max-clauses N] [--mode MODE] [--portfolios A,B,C] [--skip N] [--limit N] [--out FILE]";
  exit 2

let is_problem_file f = Filename.check_suffix f ".p"

let rec walk dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.concat_map (fun name ->
       let path = Filename.concat dir name in
       if Sys.is_directory path then walk path
       else if is_problem_file path then [ path ]
       else [])

let trim s =
  let len = String.length s in
  let first = ref 0 in
  while !first < len && (s.[!first] = ' ' || s.[!first] = '\t' || s.[!first] = '\r') do
    incr first
  done;
  let last = ref (len - 1) in
  while !last >= !first && (s.[!last] = ' ' || s.[!last] = '\t' || s.[!last] = '\r') do
    decr last
  done;
  if !last < !first then "" else String.sub s !first (!last - !first + 1)

let read_file_list ?root file =
  let ic = open_in file in
  let rec loop acc =
    match input_line ic with
    | line ->
        let line = trim line in
        if line = "" || line.[0] = '#' then loop acc
        else
          let path =
            match root with
            | Some r when Filename.is_relative line -> Filename.concat r line
            | _ -> line
          in
          loop (path :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

let csv_escape s =
  let b = Buffer.create (String.length s + 8) in
  Buffer.add_char b '"';
  String.iter
    (fun c -> if c = '"' then Buffer.add_string b "\"\"" else Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let mode_of_string = function
  | "unrestricted" -> Resolution.Unrestricted
  | "ordered" -> Resolution.Ordered
  | "ordered-fallback" -> Resolution.Ordered_with_fallback
  | _ -> usage ()

let portfolio_of_string = function
  | "legacy-modern" | "portfolio" -> Prover.Legacy_then_modern
  | "modern-legacy" | "modern-first" -> Prover.Modern_then_legacy
  | "legacy-only" | "legacy" -> Prover.Legacy_only
  | "modern-only" | "modern" -> Prover.Modern_only
  | "modern-compat-only" | "compat-only" | "compat" -> Prover.Modern_compat_only
  | "feq-modern" | "feq" | "equality" -> Prover.Feq_modern
  | "scheduled" | "scheduler" -> Prover.Scheduled_portfolio
  | "experimental-casc" | "casc-experimental" | "exp-casc" -> Prover.Experimental_casc
  | _ -> usage ()

let portfolio_name = function
  | Prover.Legacy_then_modern -> "legacy-modern"
  | Prover.Modern_then_legacy -> "modern-legacy"
  | Prover.Legacy_only -> "legacy-only"
  | Prover.Modern_only -> "modern-only"
  | Prover.Modern_compat_only -> "modern-compat-only"
  | Prover.Feq_modern -> "feq-modern"
  | Prover.Scheduled_portfolio -> "scheduled"
  | Prover.Experimental_casc -> "experimental-casc"

let split_commas s =
  s
  |> String.split_on_char ','
  |> List.map trim
  |> List.filter (fun x -> x <> "")

let status_string = Prover.string_of_szs_status

let success = function
  | Prover.Theorem | Prover.Unsatisfiable -> true
  | _ -> false

let short_problem_name root file =
  let root =
    if String.length root > 0 && root.[String.length root - 1] = Filename.dir_sep.[0]
    then root
    else root ^ Filename.dir_sep
  in
  let root_len = String.length root in
  if String.length file >= root_len && String.sub file 0 root_len = root then
    String.sub file root_len (String.length file - root_len)
  else file

let ensure_parent_dir file =
  let dir = Filename.dirname file in
  if dir <> "." && not (Sys.file_exists dir) then Unix.mkdir dir 0o755

let empty_stats =
  {
    Resolution.generated_clauses = 0;
    processed_clauses = 0;
    resolution_inferences = 0;
    factoring_inferences = 0;
    equality_resolution_inferences = 0;
    equality_factoring_inferences = 0;
    superposition_inferences = 0;
    demodulation_rewrites = 0;
    subsumption_tests = 0;
    subsumption_rejections = 0;
    avatar_enabled = false;
    avatar_keep_original = false;
    avatar_min_split_literals = 0;
    avatar_max_split_vars = 0;
    avatar_split_vars_used = 0;
    avatar_split_attempts = 0;
    avatar_successful_splits = 0;
    avatar_split_components = 0;
    avatar_split_rejected_disabled = 0;
    avatar_split_rejected_short = 0;
    avatar_split_rejected_equality = 0;
    avatar_split_rejected_nonground = 0;
    avatar_split_rejected_trivial = 0;
    avatar_split_rejected_quota = 0;
    avatar_contextual_empty_conflicts = 0;
    avatar_sat_clauses_added = 0;
    avatar_sat_solves = 0;
    avatar_sat_conflicts = 0;
    avatar_context_sat_tests = 0;
    avatar_context_sat_failures = 0;
    avatar_filtered_inferences = 0;
    wall_clock_s = 0.0;
  }

let run_one ~base_config portfolio_mode file =
  let config = { base_config with Prover.portfolio_mode } in
  try Prover.run_file ~config file
  with exn ->
    let _ = exn in
    let info =
      {
        Prover.file = Some file;
        clause_count = 0;
        generated_clause_count = 0;
        profile = "unknown";
        raw_clause_count = 0;
        equality_literals = 0;
        equality_literal_ratio = 0.0;
        avg_literal_term_size = 0.0;
        unit_ratio = 0.0;
        negative_ratio = 0.0;
        axiom_selection_enabled = false;
      }
    in
    {
      Prover.status = InputError;
      info;
      derivation = [];
      empty_clause = None;
      resolution_stats = Some empty_stats;
    }

let stats_of outcome =
  match outcome.Prover.resolution_stats with
  | Some stats -> stats
  | None -> empty_stats

let new_summary () =
  {
    solved = 0;
    timeout = 0;
    input_error = 0;
    other_failure = 0;
    total_time = 0.0;
    generated = 0;
    processed = 0;
  }

let update_summary summary outcome actual_time =
  let stats = stats_of outcome in
  (if success outcome.Prover.status then summary.solved <- summary.solved + 1
   else
     match outcome.status with
     | Timeout -> summary.timeout <- summary.timeout + 1
     | InputError -> summary.input_error <- summary.input_error + 1
     | _ -> summary.other_failure <- summary.other_failure + 1);
  summary.total_time <- summary.total_time +. actual_time;
  summary.generated <- summary.generated + stats.generated_clauses;
  summary.processed <- summary.processed + stats.processed_clauses

let () =
  let dir = ref None in
  let file_list = ref None in
  let root = ref None in
  let time_limit = ref None in
  let tptp_dir = ref None in
  let max_clauses = ref None in
  let mode = ref Resolution.Ordered_with_fallback in
  let skip = ref 0 in
  let limit = ref None in
  let out = ref "bench/logs/mode_compare.csv" in
  let portfolios =
    ref
      [
        Prover.Feq_modern;
        Prover.Experimental_casc;
        Prover.Legacy_then_modern;
        Prover.Modern_then_legacy;
        Prover.Scheduled_portfolio;
      ]
  in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--dir" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          dir := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--file-list" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          file_list := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--root" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          root := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--time-limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          time_limit := Some (float_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--tptp" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          tptp_dir := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--max-clauses" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          max_clauses := Some (int_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--mode" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          mode := mode_of_string Sys.argv.(i + 1);
          loop (i + 2)
      | "--portfolios" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          portfolios := split_commas Sys.argv.(i + 1) |> List.map portfolio_of_string;
          loop (i + 2)
      | "--skip" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          skip := int_of_string Sys.argv.(i + 1);
          if !skip < 0 then usage ();
          loop (i + 2)
      | "--limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          limit := Some (int_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--out" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          out := Sys.argv.(i + 1);
          loop (i + 2)
      | _ -> usage ()
  in
  loop 1;

  let time_limit = match !time_limit with Some t -> t | None -> usage () in
  let files =
    match (!dir, !file_list) with
    | Some dir, None -> walk dir
    | None, Some list -> read_file_list ?root:!root list
    | _ -> usage ()
  in
  let short_root =
    match (!root, !dir) with
    | Some root, _ -> root
    | None, Some dir -> dir
    | None, None -> Filename.dirname (List.hd files)
  in
  let files = files |> List.to_seq |> Seq.drop !skip in
  let files =
    match !limit with
    | None -> files
    | Some n -> Seq.take n files
  in
  let files = List.of_seq files in
  if files = [] || !portfolios = [] then usage ();

  ensure_parent_dir !out;
  let base_config =
    {
      Prover.default_config with
      time_limit_s = Some time_limit;
      max_generated_clauses = !max_clauses;
      inference_mode = !mode;
      tptp_dir = !tptp_dir;
      use_sos = true;
      print_derivation = false;
    }
  in
  let summaries = Hashtbl.create 8 in
  List.iter (fun p -> Hashtbl.add summaries (portfolio_name p) (new_summary ())) !portfolios;

  let oc = open_out !out in
  Printf.fprintf oc
    "problem,portfolio,status,success,time_s,engine_time_s,profile,raw_clauses,clauses,equality_literals,equality_ratio,avg_literal_term_size,unit_ratio,negative_ratio,axiom_selection,generated,processed,resolution_inferences,factoring_inferences,equality_resolution_inferences,equality_factoring_inferences,superposition_inferences,demodulation_rewrites,subsumption_tests,subsumption_rejections,avatar_enabled,avatar_splits,avatar_components,avatar_vars_used,avatar_max_vars,avatar_filtered_inferences,avatar_context_failures,avatar_sat_conflicts\n";
  flush oc;

  let total = List.length files in
  Printf.eprintf "Comparing %d problem(s), %d portfolio mode(s), time_limit=%.3fs, output=%s\n%!"
    total
    (List.length !portfolios)
    time_limit
    !out;
  List.iteri
    (fun idx file ->
      let problem = short_problem_name short_root file in
      Printf.eprintf "[%d/%d] %s\n%!" (idx + 1) total problem;
      List.iter
        (fun portfolio ->
          let name = portfolio_name portfolio in
          let started = Unix.gettimeofday () in
          let outcome = run_one ~base_config portfolio file in
          let actual_time = Unix.gettimeofday () -. started in
          let stats = stats_of outcome in
          let info = outcome.Prover.info in
          update_summary (Hashtbl.find summaries name) outcome actual_time;
          Printf.fprintf oc
            "%s,%s,%s,%b,%.6f,%.6f,%s,%d,%d,%d,%.6f,%.6f,%.6f,%.6f,%b,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%b,%d,%d,%d,%d,%d,%d,%d\n"
            (csv_escape problem)
            (csv_escape name)
            (csv_escape (status_string outcome.status))
            (success outcome.status)
            actual_time
            stats.wall_clock_s
            (csv_escape info.profile)
            info.raw_clause_count
            info.clause_count
            info.equality_literals
            info.equality_literal_ratio
            info.avg_literal_term_size
            info.unit_ratio
            info.negative_ratio
            info.axiom_selection_enabled
            stats.generated_clauses
            stats.processed_clauses
            stats.resolution_inferences
            stats.factoring_inferences
            stats.equality_resolution_inferences
            stats.equality_factoring_inferences
            stats.superposition_inferences
            stats.demodulation_rewrites
            stats.subsumption_tests
            stats.subsumption_rejections
            stats.avatar_enabled
            stats.avatar_successful_splits
            stats.avatar_split_components
            stats.avatar_split_vars_used
            stats.avatar_max_split_vars
            stats.avatar_filtered_inferences
            stats.avatar_context_sat_failures
            stats.avatar_sat_conflicts;
          flush oc;
          Printf.eprintf "      %-18s %s %.3fs gen=%d proc=%d\n%!"
            name
            (status_string outcome.status)
            actual_time
            stats.generated_clauses
            stats.processed_clauses)
        !portfolios)
    files;
  close_out oc;

  Printf.eprintf "\nSummary:\n%!";
  List.iter
    (fun portfolio ->
      let name = portfolio_name portfolio in
      let s = Hashtbl.find summaries name in
      Printf.eprintf
        "  %-18s solved=%d timeout=%d input_error=%d other=%d time=%.3fs gen=%d proc=%d\n%!"
        name
        s.solved
        s.timeout
        s.input_error
        s.other_failure
        s.total_time
        s.generated
        s.processed)
    !portfolios;
  Printf.eprintf "CSV written to: %s\n%!" !out
