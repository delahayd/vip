type row = {
  problem : string;
  expected : string;
  ip_status : string;
}

type bench = {
  file : string;
  date : string;
  rows : row list;
}

let starts_with s prefix =
  let ls = String.length s in
  let lp = String.length prefix in
  ls >= lp && String.sub s 0 lp = prefix

let csv_split line =
  let len = String.length line in
  let rec aux i in_quotes field acc =
    if i >= len then
      List.rev (Buffer.contents field :: acc)
    else
      match line.[i] with
      | '"' ->
          if in_quotes && i + 1 < len && line.[i + 1] = '"' then begin
            Buffer.add_char field '"';
            aux (i + 2) in_quotes field acc
          end else
            aux (i + 1) (not in_quotes) field acc
      | ',' when not in_quotes ->
          let v = Buffer.contents field in
          Buffer.clear field;
          aux (i + 1) in_quotes field (v :: acc)
      | c ->
          Buffer.add_char field c;
          aux (i + 1) in_quotes field acc
  in
  aux 0 false (Buffer.create 32) []

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

let file_uri path =
  let abs =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  "file://" ^ abs

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

let is_ip_success expected actual =
  let expected_unsat =
    match expected with
    | "Theorem" | "Unsatisfiable" | "ContradictoryAxioms" -> true
    | _ -> false
  in
  let expected_sat =
    match expected with
    | "Satisfiable" | "CounterSatisfiable" -> true
    | _ -> false
  in
  let actual_unsat =
    match actual with
    | "Theorem" | "Unsatisfiable" | "ContradictoryAxioms" -> true
    | _ -> false
  in
  let actual_sat =
    match actual with
    | "Satisfiable" | "CounterSatisfiable" -> true
    | _ -> false
  in
  if expected_unsat then actual_unsat
  else if expected_sat then actual_sat || actual = "Timeout"
  else
    match actual with
    | "Theorem" | "Unsatisfiable" | "ContradictoryAxioms"
    | "Satisfiable" | "CounterSatisfiable" -> true
    | _ -> false

let assoc_index name header =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if x = name then Some i else aux (i + 1) xs
  in
  aux 0 header

let nth_opt xs n =
  if n < 0 then None else
  let rec aux i = function
    | [] -> None
    | x :: xs -> if i = n then Some x else aux (i + 1) xs
  in
  aux 0 xs

let read_bench file =
  let ic = open_in file in
  let date = ref "Unknown" in
  let header = ref None in
  let rows = ref [] in

  begin
    try
      while true do
        let line = input_line ic in
        if starts_with line "# bench_date," then begin
          match csv_split line with
          | _ :: d :: _ -> date := d
          | _ -> ()
        end else if starts_with line "section,problem," then
          header := Some (csv_split line)
        else if starts_with line "result," then
          match !header with
          | None -> ()
          | Some h ->
              let cols = csv_split line in
              let problem_i = assoc_index "problem" h in
              let expected_i = assoc_index "expected_status" h in
              let ip_i = assoc_index "ip" h in
              begin
                match problem_i, expected_i, ip_i with
                | Some pi, Some ei, Some ii ->
                    begin
                      match nth_opt cols pi, nth_opt cols ei, nth_opt cols ii with
                      | Some problem, Some expected, Some ip_status ->
                          rows := { problem; expected; ip_status } :: !rows
                      | _ -> ()
                    end
                | _ -> ()
              end
      done
    with End_of_file ->
      close_in ic
  end;

  { file; date = !date; rows = List.rev !rows }

let row_map rows =
  let tbl = Hashtbl.create 4096 in
  List.iter (fun r -> Hashtbl.replace tbl r.problem r) rows;
  tbl

let output_path () =
  if not (Sys.file_exists "bench") then Unix.mkdir "bench" 0o755;
  if not (Sys.file_exists "bench/logs") then Unix.mkdir "bench/logs" 0o755;
  Filename.concat "bench/logs" ("regression_" ^ now_stamp () ^ ".html")

let write_html path old_b new_b common_count regressions =
  let oc = open_out path in
  Printf.fprintf oc
    "<!doctype html><html><head><meta charset=\"utf-8\"/>\
     <title>Regression ip</title>\
     <style>\
     body{font-family:sans-serif;margin:2rem;}\
     table{border-collapse:collapse;width:100%%;}\
     th,td{border:1px solid #ccc;padding:.35rem .5rem;}\
     th{background:#eee;}\
     .ok{background:#c8f7c5;}\
     .fail{background:#ffc9c9;}\
     a{color:#0645ad;text-decoration:none;}\
     a:hover{text-decoration:underline;}\
     </style></head><body>";

  Printf.fprintf oc "<h1>Test de régression ip</h1>";
  Printf.fprintf oc "<p>Date du test : %s</p>" (html_escape (now_stamp ()));

  Printf.fprintf oc
    "<table>\
     <tr><th>Fichier</th><th>Date</th></tr>\
     <tr><td>%s</td><td>%s</td></tr>\
     <tr><td>%s</td><td>%s</td></tr>\
     </table>"
    (html_escape old_b.file)
    (html_escape old_b.date)
    (html_escape new_b.file)
    (html_escape new_b.date);

  if common_count = 0 then
    Printf.fprintf oc
      "<h2>Aucun fichier de problème en commun</h2>\
       <p>Aucun test de régression n’a pu être effectué.</p>"
  else if regressions = [] then
    Printf.fprintf oc
      "<h2 class=\"ok\">Aucune régression</h2>\
       <p>%d problème(s) commun(s) ont été comparés.</p>"
      common_count
  else begin
    Printf.fprintf oc
      "<h2 class=\"fail\">Régressions détectées</h2>\
       <p>%d problème(s) commun(s), %d régression(s).</p>"
      common_count
      (List.length regressions);

    Printf.fprintf oc
      "<table><tr>\
       <th>Problème</th><th>Statut attendu</th>\
       <th>Ancien statut ip</th><th>Nouveau statut ip</th>\
       </tr>";

    List.iter
      (fun (old_r, new_r) ->
        let link =
          Printf.sprintf "<a href=\"%s\">%s</a>"
            (html_escape (file_uri new_r.problem))
            (html_escape new_r.problem)
        in
        Printf.fprintf oc
          "<tr><td>%s</td><td>%s</td><td class=\"ok\">%s</td><td class=\"fail\">%s</td></tr>"
          link
          (html_escape new_r.expected)
          (html_escape old_r.ip_status)
          (html_escape new_r.ip_status))
      regressions;

    Printf.fprintf oc "</table>"
  end;

  Printf.fprintf oc "</body></html>\n";
  close_out oc

let usage () =
  prerr_endline "Usage: check_regression <old_results.csv> <new_results.csv>";
  exit 2

let () =
  if Array.length Sys.argv <> 3 then usage ();

  let b1 = read_bench Sys.argv.(1) in
  let b2 = read_bench Sys.argv.(2) in

  let old_b, new_b =
    if String.compare b1.date b2.date <= 0 then b1, b2 else b2, b1
  in

  let old_map = row_map old_b.rows in
  let common_count = ref 0 in
  let regressions = ref [] in

  List.iter
    (fun new_r ->
      match Hashtbl.find_opt old_map new_r.problem with
      | None -> ()
      | Some old_r ->
          incr common_count;
          let old_ok = is_ip_success old_r.expected old_r.ip_status in
          let new_ok = is_ip_success new_r.expected new_r.ip_status in
          if old_ok && not new_ok then
            regressions := (old_r, new_r) :: !regressions)
    new_b.rows;

  let out = output_path () in
  write_html out old_b new_b !common_count (List.rev !regressions);

  if !common_count = 0 then
    Printf.printf "Aucun fichier de problème en commun. HTML written to: %s\n" out
  else if !regressions = [] then
    Printf.printf "Aucune régression. HTML written to: %s\n" out
  else
    Printf.printf "%d régression(s). HTML written to: %s\n"
      (List.length !regressions) out
