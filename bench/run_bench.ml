type prover_kind =
  | Ip
  | External

type prover = {
  name : string;
  kind : prover_kind;
  cmd : string -> int -> int option -> string option -> string;
  version_cmd : string;
}

type result = {
  status : string;
  raw : string;
  time_s : float;
}

type config = {
  dir : string;
  time_limit : int;
  max_clauses : int option;
  mode : string option;
  debug : bool;
  only_ip : bool;
}

type bench_row = {
  problem : string;
  expected_status : string;
  results : (string * result) list;
}

let provers =
  [
    {
      name = "ip";
      kind = Ip;
      cmd =
        (fun file timeout max_clauses mode ->
          let max_clause_arg =
            match max_clauses with
            | None -> ""
            | Some n -> Printf.sprintf " --max-clauses %d" n
          in
          let mode_arg =
            match mode with
            | None -> ""
            | Some m -> Printf.sprintf " --mode %s" m
          in
          Printf.sprintf
            "./bin/ip --time-limit %d%s%s %s"
            timeout
            max_clause_arg
            mode_arg
            file);
      version_cmd = "git rev-parse --short HEAD";
    };
    {
      name = "vampire";
      kind = External;
      cmd =
        (fun file timeout _max_clauses _mode ->
          Printf.sprintf "vampire --mode casc --time_limit %d %s" timeout file);
      version_cmd = "vampire --version";
    };
    {
      name = "e";
      kind = External;
      cmd =
        (fun file timeout _max_clauses _mode ->
          Printf.sprintf "eprover --auto --cpu-limit=%d %s" timeout file);
      version_cmd = "eprover --version";
    };
    {
      name = "zenon";
      kind = External;
      cmd =
        (fun file timeout _max_clauses _mode ->
          Printf.sprintf "zenon -itptp -max-time %d %s" timeout file);
      version_cmd = "zenon -v";
    };
  ]

let active_provers config =
  if config.only_ip then
    List.filter (fun p -> p.name = "ip") provers
  else
    provers

let interrupted = ref false

let starts_with s prefix =
  let ls = String.length s in
  let lp = String.length prefix in
  ls >= lp && String.sub s 0 lp = prefix

let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in
  aux 0

let csv_escape s =
  let b = Buffer.create (String.length s + 8) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\"\""
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let html_escape s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let usage () =
  prerr_endline
    "Usage: run_bench [--debug] [--onlyip] [--dir DIR] --time-limit SECONDS [--max-clauses N] [--mode MODE]";
  exit 2

let parse_args () =
  let dir = ref "." in
  let time_limit = ref None in
  let max_clauses = ref None in
  let mode = ref None in
  let debug = ref false in
  let only_ip = ref false in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--debug" ->
          debug := true;
          loop (i + 1)
      | "--onlyip" ->
          only_ip := true;
          loop (i + 1)
      | "--dir" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          dir := Sys.argv.(i + 1);
          loop (i + 2)
      | "--time-limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          let n =
            try int_of_string Sys.argv.(i + 1)
            with Failure _ -> usage ()
          in
          time_limit := Some n;
          loop (i + 2)
      | "--max-clauses" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          let n =
            try int_of_string Sys.argv.(i + 1)
            with Failure _ -> usage ()
          in
          max_clauses := Some n;
          loop (i + 2)
      | "--mode" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          mode := Some Sys.argv.(i + 1);
          loop (i + 2)
      | _ ->
          usage ()
  in

  loop 1;

  let time_limit =
    match !time_limit with
    | Some n -> n
    | None -> usage ()
  in

  {
    dir = !dir;
    time_limit;
    max_clauses = !max_clauses;
    mode = !mode;
    debug = !debug;
    only_ip = !only_ip;
  }

let is_problem_file f =
  Filename.check_suffix f ".p"

let rec walk dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.concat_map (fun name ->
       let path = Filename.concat dir name in
       if Sys.is_directory path then walk path
       else if is_problem_file path then [ path ]
       else [])

let read_all ic =
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_string buf (input_line ic);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf

let run_command command =
  let ic = Unix.open_process_in (command ^ " 2>&1") in
  let output = read_all ic in
  let status = Unix.close_process_in ic in
  output, status

let exit_code_of_status = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

let first_line s =
  match String.split_on_char '\n' s with
  | [] -> ""
  | x :: _ -> String.trim x

let safe_command_output cmd =
  try
    let output, _ = run_command cmd in
    first_line output
  with _ ->
    "unavailable"

let now_stamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf
    "%04d-%02d-%02d_%02d-%02d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let ensure_logs_dir () =
  if not (Sys.file_exists "bench") then Unix.mkdir "bench" 0o755;
  if not (Sys.file_exists "bench/logs") then Unix.mkdir "bench/logs" 0o755

let unique_paths stamp =
  ensure_logs_dir ();
  let rec loop n =
    let suffix = if n = 0 then "" else Printf.sprintf "_%d" n in
    let csv_path = Printf.sprintf "bench/logs/bench_%s%s.csv" stamp suffix in
    let html_path = Printf.sprintf "bench/logs/bench_%s%s.html" stamp suffix in
    if Sys.file_exists csv_path || Sys.file_exists html_path then loop (n + 1)
    else csv_path, html_path
  in
  loop 0

let file_uri path =
  let abs =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  "file://" ^ abs

let extract_status output =
  let lines = String.split_on_char '\n' output in
  let find_szs () =
    List.find_opt
      (fun l -> starts_with l "% SZS status " || starts_with l "SZS status ")
      lines
  in
  match find_szs () with
  | Some l ->
      begin
        match String.split_on_char ' ' l with
        | "%" :: "SZS" :: "status" :: status :: _ -> status
        | "SZS" :: "status" :: status :: _ -> status
        | _ -> "Unknown"
      end
  | None ->
      let lower = String.lowercase_ascii output in
      if contains_substring output "(* PROOF-FOUND *)" then
        "Unsatisfiable"
      else if contains_substring output "(* NO-PROOF *)" then
        if contains_substring lower "time limit"
           || contains_substring lower
                "could not find a proof within the time limit"
        then
          "Timeout"
        else
          "Satisfiable"
      else if contains_substring lower "timeout"
           || contains_substring lower "time limit"
      then
        "Timeout"
      else
        "Unknown"

let expected_unsat_like = function
  | "Theorem" | "Unsatisfiable" | "ContradictoryAxioms" -> true
  | _ -> false

let expected_sat_like = function
  | "Satisfiable" | "CounterSatisfiable" -> true
  | _ -> false

let result_unsat_like = function
  | "Theorem" | "Unsatisfiable" | "ContradictoryAxioms" -> true
  | _ -> false

let result_sat_like = function
  | "Satisfiable" | "CounterSatisfiable" -> true
  | _ -> false

let is_success = function
  | "Theorem"
  | "Unsatisfiable"
  | "CounterSatisfiable"
  | "Satisfiable"
  | "ContradictoryAxioms" -> true
  | _ -> false

let is_incomplete expected actual =
  expected_unsat_like expected && result_sat_like actual

let is_unsound expected actual =
  expected_sat_like expected && result_unsat_like actual

let is_correct_success expected actual =
  if expected_unsat_like expected then result_unsat_like actual
  else if expected_sat_like expected then
    result_sat_like actual || actual = "Timeout"
  else
    is_success actual

let is_counted_timeout expected actual =
  actual = "Timeout" && not (expected_sat_like expected)

let run_prover config prover file =
  let cmd =
    prover.cmd file config.time_limit config.max_clauses config.mode
  in
  let t0 = Unix.gettimeofday () in
  let output, status = run_command cmd in
  let time_s = Unix.gettimeofday () -. t0 in
  let code = exit_code_of_status status in
  let parsed_status = extract_status output in

  let status_str =
    if parsed_status <> "Unknown" then parsed_status
    else if code <> 0 then "Error"
    else "Unknown"
  in

  if config.debug && status_str = "Error" then
    Printf.eprintf
      "\n[DEBUG] Error on %s with %s\nCommand: %s\nExit code: %d\nOutput:\n%s\n%!"
      file prover.name cmd code output;

  { status = status_str; raw = output; time_s }

let read_problem_expected_status file =
  try
    let ic = open_in file in
    let rec loop () =
      match input_line ic with
      | line ->
          let line = String.trim line in
          if starts_with line "% Status" then begin
            close_in_noerr ic;
            match String.split_on_char ':' line with
            | _ :: rest -> String.trim (String.concat ":" rest)
            | _ -> "Unknown"
          end
          else loop ()
      | exception End_of_file ->
          close_in_noerr ic;
          "Unknown"
    in
    loop ()
  with _ ->
    "Unknown"

let result_status name results =
  match List.assoc_opt name results with
  | Some r -> r.status
  | None -> "Missing"

let result_time name results =
  match List.assoc_opt name results with
  | Some r -> r.time_s
  | None -> 0.0

let write_metadata oc ~stamp ~config ~versions =
  Printf.fprintf oc "# bench_date,%s\n" (csv_escape stamp);
  Printf.fprintf oc "# problem_dir,%s\n" (csv_escape config.dir);
  Printf.fprintf oc "# time_limit_seconds,%d\n" config.time_limit;
  begin
    match config.max_clauses with
    | None -> Printf.fprintf oc "# max_clauses,\n"
    | Some n -> Printf.fprintf oc "# max_clauses,%d\n" n
  end;
  begin
    match config.mode with
    | None -> Printf.fprintf oc "# mode,\n"
    | Some m -> Printf.fprintf oc "# mode,%s\n" (csv_escape m)
  end;
  Printf.fprintf oc "# only_ip,%b\n" config.only_ip;
  List.iter
    (fun (name, version) ->
      Printf.fprintf oc "# version_%s,%s\n" name (csv_escape version))
    versions;
  Printf.fprintf oc "\n%!"

let write_results_header oc config =
  if config.only_ip then
    Printf.fprintf oc
      "section,problem,expected_status,ip,ip_time_s\n%!"
  else
    Printf.fprintf oc
      "section,problem,expected_status,ip,ip_time_s,vampire,vampire_time_s,e,e_time_s,zenon,zenon_time_s\n%!"

let write_result_row oc config row =
  if config.only_ip then
    Printf.fprintf oc
      "result,%s,%s,%s,%.6f\n%!"
      (csv_escape row.problem)
      (csv_escape row.expected_status)
      (csv_escape (result_status "ip" row.results))
      (result_time "ip" row.results)
  else
    Printf.fprintf oc
      "result,%s,%s,%s,%.6f,%s,%.6f,%s,%.6f,%s,%.6f\n%!"
      (csv_escape row.problem)
      (csv_escape row.expected_status)
      (csv_escape (result_status "ip" row.results))
      (result_time "ip" row.results)
      (csv_escape (result_status "vampire" row.results))
      (result_time "vampire" row.results)
      (csv_escape (result_status "e" row.results))
      (result_time "e" row.results)
      (csv_escape (result_status "zenon" row.results))
      (result_time "zenon" row.results)

let pair_stats rows a b =
  let win = ref 0 in
  let draw = ref 0 in
  let loss = ref 0 in

  List.iter
    (fun row ->
      let sa =
        is_correct_success row.expected_status (result_status a row.results)
      in
      let sb =
        is_correct_success row.expected_status (result_status b row.results)
      in
      if sa && not sb then incr win
      else if (sa && sb) || ((not sa) && not sb) then incr draw
      else incr loss)
    rows;

  !win, !draw, !loss

let unique_solved rows prover_name active =
  List.fold_left
    (fun acc row ->
      let mine =
        is_correct_success row.expected_status
          (result_status prover_name row.results)
      in
      let others =
        List.exists
          (fun p ->
            p.name <> prover_name
            && is_correct_success row.expected_status
                 (result_status p.name row.results))
          active
      in
      if mine && not others then acc + 1 else acc)
    0
    rows

let prover_stats rows prover_name active =
  let total = ref 0 in
  let success = ref 0 in
  let timeouts = ref 0 in
  let incomplete = ref 0 in
  let unsound = ref 0 in
  let total_time = ref 0.0 in

  List.iter
    (fun row ->
      incr total;
      let status = result_status prover_name row.results in
      let time_s = result_time prover_name row.results in
      total_time := !total_time +. time_s;
      if is_correct_success row.expected_status status then incr success;
      if is_counted_timeout row.expected_status status then incr timeouts;
      if is_incomplete row.expected_status status then incr incomplete;
      if is_unsound row.expected_status status then incr unsound)
    rows;

  let avg_time_s =
    if !total = 0 then 0.0 else !total_time /. float_of_int !total
  in
  let uniques = unique_solved rows prover_name active in

  !total, !success, !timeouts, !incomplete, !unsound, uniques, avg_time_s

let write_stats oc config rows =
  let active = active_provers config in

  if config.only_ip then
    Printf.fprintf oc
      "\nsection,prover,total,success,timeout,incomplete,unsound,avg_time_s\n"
  else
    Printf.fprintf oc
      "\nsection,prover,total,success,timeout,incomplete,unsound,uniques,avg_time_s\n";

  List.iter
    (fun p ->
      let total, success, timeouts, incomplete, unsound, uniques, avg_time_s =
        prover_stats rows p.name active
      in
      if config.only_ip then
        Printf.fprintf oc "prover_stats,%s,%d,%d,%d,%d,%d,%.6f\n"
          (csv_escape p.name)
          total
          success
          timeouts
          incomplete
          unsound
          avg_time_s
      else
        Printf.fprintf oc "prover_stats,%s,%d,%d,%d,%d,%d,%d,%.6f\n"
          (csv_escape p.name)
          total
          success
          timeouts
          incomplete
          unsound
          uniques
          avg_time_s)
    active;

  if not config.only_ip then begin
    Printf.fprintf oc "\nsection,prover_a,prover_b,win,draw,loss\n";
    List.iter
      (fun a ->
        List.iter
          (fun b ->
            if a.name <> b.name then
              let win, draw, loss = pair_stats rows a.name b.name in
              Printf.fprintf oc "pair_stats,%s,%s,%d,%d,%d\n"
                (csv_escape a.name)
                (csv_escape b.name)
                win
                draw
                loss)
          active)
      active
  end;

  Printf.fprintf oc "%!"

let html_status_cell expected status time_s =
  let cls = if is_correct_success expected status then "ok" else "fail" in
  Printf.sprintf
    "<td class=\"%s\">%s<br/><small>%.3fs</small></td>"
    cls
    (html_escape status)
    time_s

let write_html_header oc ~stamp ~config ~versions =
  Printf.fprintf oc
    "<!doctype html><html><head><meta charset=\"utf-8\" />\
     <title>Benchmark %s</title>\
     <style>\
     body{font-family:sans-serif;margin:2rem;}\
     table{border-collapse:collapse;width:100%%;}\
     th,td{border:1px solid #ccc;padding:0.35rem 0.5rem;}\
     th{background:#eee;}\
     td.ok{background:#c8f7c5;}\
     td.fail{background:#ffc9c9;}\
     td.meta{background:#f7f7f7;}\
     small{color:#333;}\
     a{color:#0645ad;text-decoration:none;}\
     a:hover{text-decoration:underline;}\
     </style></head><body>\
     <h1>Benchmark %s</h1>\
     <table>\
     <tr><th>Champ</th><th>Valeur</th></tr>\
     <tr><td>Date</td><td>%s</td></tr>\
     <tr><td>Répertoire</td><td>%s</td></tr>\
     <tr><td>Time limit</td><td>%d</td></tr>"
    (html_escape stamp)
    (html_escape stamp)
    (html_escape stamp)
    (html_escape config.dir)
    config.time_limit;

  begin
    match config.max_clauses with
    | None -> Printf.fprintf oc "<tr><td>Max clauses</td><td></td></tr>"
    | Some n -> Printf.fprintf oc "<tr><td>Max clauses</td><td>%d</td></tr>" n
  end;

  begin
    match config.mode with
    | None -> Printf.fprintf oc "<tr><td>Mode</td><td></td></tr>"
    | Some m ->
        Printf.fprintf oc "<tr><td>Mode</td><td>%s</td></tr>" (html_escape m)
  end;

  Printf.fprintf oc "<tr><td>Only ip</td><td>%b</td></tr>" config.only_ip;

  List.iter
    (fun (name, version) ->
      Printf.fprintf oc "<tr><td>Version %s</td><td>%s</td></tr>"
        (html_escape name)
        (html_escape version))
    versions;

  if config.only_ip then
    Printf.fprintf oc
      "</table><h2>Résultats</h2>\
       <table><tr>\
       <th>Problème</th><th>Status attendu</th><th>ip</th>\
       </tr>%!"
  else
    Printf.fprintf oc
      "</table><h2>Résultats</h2>\
       <table><tr>\
       <th>Problème</th><th>Status attendu</th>\
       <th>ip</th><th>Vampire</th><th>E</th><th>Zenon</th>\
       </tr>%!"

let write_html_row oc config row =
  let problem_link =
    Printf.sprintf
      "<a href=\"%s\">%s</a>"
      (html_escape (file_uri row.problem))
      (html_escape row.problem)
  in
  if config.only_ip then
    Printf.fprintf oc
      "<tr><td>%s</td><td class=\"meta\">%s</td>%s</tr>\n%!"
      problem_link
      (html_escape row.expected_status)
      (html_status_cell row.expected_status
         (result_status "ip" row.results)
         (result_time "ip" row.results))
  else
    Printf.fprintf oc
      "<tr><td>%s</td><td class=\"meta\">%s</td>%s%s%s%s</tr>\n%!"
      problem_link
      (html_escape row.expected_status)
      (html_status_cell row.expected_status
         (result_status "ip" row.results)
         (result_time "ip" row.results))
      (html_status_cell row.expected_status
         (result_status "vampire" row.results)
         (result_time "vampire" row.results))
      (html_status_cell row.expected_status
         (result_status "e" row.results)
         (result_time "e" row.results))
      (html_status_cell row.expected_status
         (result_status "zenon" row.results)
         (result_time "zenon" row.results))

let write_html_stats oc config rows =
  let active = active_provers config in

  if config.only_ip then
    Printf.fprintf oc
      "</table><h2>Stats par prouveur</h2>\
       <table><tr>\
       <th>Prouveur</th><th>Total</th><th>Succès</th><th>Timeouts</th>\
       <th>Incomplétude</th><th>Incorrection</th><th>Temps moyen</th>\
       </tr>"
  else
    Printf.fprintf oc
      "</table><h2>Stats par prouveur</h2>\
       <table><tr>\
       <th>Prouveur</th><th>Total</th><th>Succès</th><th>Timeouts</th>\
       <th>Incomplétude</th><th>Incorrection</th><th>Uniques</th>\
       <th>Temps moyen</th>\
       </tr>";

  List.iter
    (fun p ->
      let total, success, timeouts, incomplete, unsound, uniques, avg_time_s =
        prover_stats rows p.name active
      in
      if config.only_ip then
        Printf.fprintf oc
          "<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%.3fs</td></tr>"
          (html_escape p.name)
          total
          success
          timeouts
          incomplete
          unsound
          avg_time_s
      else
        Printf.fprintf oc
          "<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%.3fs</td></tr>"
          (html_escape p.name)
          total
          success
          timeouts
          incomplete
          unsound
          uniques
          avg_time_s)
    active;

  Printf.fprintf oc "</table>";

  if not config.only_ip then begin
    Printf.fprintf oc
      "<h2>Stats comparatives</h2>\
       <table><tr><th>A</th><th>B</th><th>win</th><th>draw</th><th>loss</th></tr>";

    List.iter
      (fun a ->
        List.iter
          (fun b ->
            if a.name <> b.name then
              let win, draw, loss = pair_stats rows a.name b.name in
              Printf.fprintf oc
                "<tr><td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td></tr>"
                (html_escape a.name)
                (html_escape b.name)
                win
                draw
                loss)
          active)
      active;

    Printf.fprintf oc "</table>"
  end;

  Printf.fprintf oc "</body></html>\n%!"

let print_header config =
  if config.only_ip then begin
    Printf.printf "%-60s | %-18s | %-22s\n%!"
      "problem" "expected" "ip";
    Printf.printf "%s\n%!" (String.make 106 '-')
  end else begin
    Printf.printf
      "%-60s | %-18s | %-22s | %-22s | %-22s | %-22s\n%!"
      "problem" "expected" "ip" "vampire" "e" "zenon";
    Printf.printf "%s\n%!" (String.make 164 '-')
  end

let print_status_with_time name row =
  Printf.sprintf "%s (%.3fs)"
    (result_status name row.results)
    (result_time name row.results)

let print_row config row =
  if config.only_ip then
    Printf.printf "%-60s | %-18s | %-22s\n%!"
      row.problem
      row.expected_status
      (print_status_with_time "ip" row)
  else
    Printf.printf
      "%-60s | %-18s | %-22s | %-22s | %-22s | %-22s\n%!"
      row.problem
      row.expected_status
      (print_status_with_time "ip" row)
      (print_status_with_time "vampire" row)
      (print_status_with_time "e" row)
      (print_status_with_time "zenon" row)

let () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         interrupted := true;
         prerr_endline "\n[INFO] Ctrl-C reçu, arrêt après le problème courant..."));

  let config = parse_args () in
  let active = active_provers config in
  let stamp = now_stamp () in
  let csv_path, html_path = unique_paths stamp in
  let versions =
    List.map (fun p -> p.name, safe_command_output p.version_cmd) active
  in

  let files = walk config.dir in
  let rows = ref [] in

  let csv_oc = open_out csv_path in
  let html_oc = open_out html_path in

  write_metadata csv_oc ~stamp ~config ~versions;
  write_results_header csv_oc config;
  write_html_header html_oc ~stamp ~config ~versions;
  print_header config;

  begin
    try
      List.iter
        (fun file ->
          if not !interrupted then begin
            let expected_status = read_problem_expected_status file in
            let results =
              List.map
                (fun prover ->
                  let r =
                    try run_prover config prover file
                    with exn ->
                      if config.debug then
                        Printf.eprintf
                          "\n[DEBUG] Exception on %s with %s: %s\n%!"
                          file
                          prover.name
                          (Printexc.to_string exn);
                      { status = "Error"; raw = ""; time_s = 0.0 }
                  in
                  prover.name, r)
                active
            in
            let row = { problem = file; expected_status; results } in
            rows := row :: !rows;
            write_result_row csv_oc config row;
            write_html_row html_oc config row;
            flush csv_oc;
            flush html_oc;
            print_row config row
          end)
        files
    with exn ->
      if config.debug then
        Printf.eprintf "\n[DEBUG] Fatal exception: %s\n%!"
          (Printexc.to_string exn)
  end;

  let rows_done = List.rev !rows in
  write_stats csv_oc config rows_done;
  write_html_stats html_oc config rows_done;
  close_out csv_oc;
  close_out html_oc;

  Printf.printf "\nCSV written to: %s\nHTML written to: %s\n%!" csv_path html_path
