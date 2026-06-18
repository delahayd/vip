open Prover_lib

let usage () =
  prerr_endline
    "Usage: compare_portfolio --dir DIR --time-limit SECONDS [--tptp DIR] [--max-clauses N] [--mode MODE] [--skip N] [--limit N] [--out FILE]";
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

let csv_escape s =
  let b = Buffer.create (String.length s + 8) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      if c = '"' then Buffer.add_string b "\"\"" else Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let mode_of_string = function
  | "unrestricted" -> Resolution.Unrestricted
  | "ordered" -> Resolution.Ordered
  | "ordered-fallback" -> Resolution.Ordered_with_fallback
  | _ -> usage ()

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

let run_one ~base_config portfolio_mode file =
  let config = { base_config with Prover.portfolio_mode } in
  try Prover.run_file ~config file
  with exn ->
    let info = {
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
    } in
    let _ = exn in
    {
      Prover.status = InputError;
      info;
      derivation = [];
      empty_clause = None;
      resolution_stats = None;
      tstp_prelude = None;
    }

let () =
  let dir = ref None in
  let time_limit = ref None in
  let tptp_dir = ref None in
  let max_clauses = ref None in
  let mode = ref Resolution.Ordered_with_fallback in
  let skip = ref 0 in
  let limit = ref None in
  let out = ref "bench/logs/portfolio_compare.csv" in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--dir" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          dir := Some Sys.argv.(i + 1);
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

  let dir = match !dir with Some d -> d | None -> usage () in
  let time_limit = match !time_limit with Some t -> t | None -> usage () in
  let files = walk dir in
  let files = files |> List.to_seq |> Seq.drop !skip in
  let files =
    match !limit with
    | None -> files
    | Some n -> Seq.take n files
  in
  let files = List.of_seq files in

  if not (Sys.file_exists "bench") then Unix.mkdir "bench" 0o755;
  if not (Sys.file_exists "bench/logs") then Unix.mkdir "bench/logs" 0o755;

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

  let oc = open_out !out in
  Printf.fprintf oc
    "problem,legacy_status,modern_compat_status,modern_status,legacy_time,modern_compat_time,modern_time,legacy_not_compat,compat_not_legacy,legacy_not_modern,modern_not_legacy\n";
  flush oc;

  let only_legacy = ref 0 in
  let compat_gap = ref 0 in
  let total = List.length files in
  Printf.eprintf "Comparing %d problem(s), skip=%d, time_limit=%.3fs per engine, output=%s\n%!"
    total
    !skip
    time_limit
    !out;
  List.iteri
    (fun idx file ->
      Printf.eprintf "[%d/%d] %s\n%!" (idx + 1) total (short_problem_name dir file);
      let legacy = run_one ~base_config Prover.Legacy_only file in
      let compat = run_one ~base_config Prover.Modern_compat_only file in
      let modern = run_one ~base_config Prover.Modern_only file in
      let time outcome =
        match outcome.Prover.resolution_stats with
        | None -> 0.0
        | Some s -> s.Resolution.wall_clock_s
      in
      let legacy_ok = success legacy.status in
      let compat_ok = success compat.status in
      let modern_ok = success modern.status in
      if legacy_ok && not compat_ok then incr compat_gap;
      if legacy_ok && not modern_ok then incr only_legacy;
      Printf.fprintf oc "%s,%s,%s,%s,%.6f,%.6f,%.6f,%b,%b,%b,%b\n"
        (csv_escape file)
        (csv_escape (status_string legacy.status))
        (csv_escape (status_string compat.status))
        (csv_escape (status_string modern.status))
        (time legacy)
        (time compat)
        (time modern)
        (legacy_ok && not compat_ok)
        (compat_ok && not legacy_ok)
        (legacy_ok && not modern_ok)
        (modern_ok && not legacy_ok);
      flush oc;
      Printf.eprintf
        "      legacy=%s compat=%s modern=%s legacy_not_compat=%b legacy_not_modern=%b\n%!"
        (status_string legacy.status)
        (status_string compat.status)
        (status_string modern.status)
        (legacy_ok && not compat_ok)
        (legacy_ok && not modern_ok))
    files;
  close_out oc;
  Printf.eprintf
    "Compared %d problem(s). legacy_not_compat=%d legacy_not_modern=%d CSV written to: %s\n%!"
    (List.length files)
    !compat_gap
    !only_legacy
    !out
