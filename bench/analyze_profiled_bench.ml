type row = {
  problem : string;
  expected : string;
  ip_status : string;
  ip_time_s : float;
  ip_profile : string;
  ip_axiom_selection : bool option;
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

let assoc_index name header =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if x = name then Some i else aux (i + 1) xs
  in
  aux 0 header

let nth_opt xs n =
  if n < 0 then None
  else
    let rec aux i = function
      | [] -> None
      | x :: xs -> if i = n then Some x else aux (i + 1) xs
    in
    aux 0 xs

let opt_col header cols name =
  match assoc_index name header with
  | None -> None
  | Some i -> nth_opt cols i

let float_col header cols name =
  match opt_col header cols name with
  | Some s -> (try float_of_string s with Failure _ -> 0.0)
  | None -> 0.0

let bool_col header cols name =
  match opt_col header cols name with
  | Some "true" -> Some true
  | Some "false" -> Some false
  | _ -> None

let string_col header cols name default =
  match opt_col header cols name with
  | Some s -> s
  | None -> default

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
              begin
                match opt_col h cols "problem",
                      opt_col h cols "expected_status",
                      opt_col h cols "ip" with
                | Some problem, Some expected, Some ip_status ->
                    rows :=
                      {
                        problem;
                        expected;
                        ip_status;
                        ip_time_s = float_col h cols "ip_time_s";
                        ip_profile = string_col h cols "ip_profile" "unknown";
                        ip_axiom_selection = bool_col h cols "ip_axiom_selection";
                      }
                      :: !rows
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
  else if expected_sat then actual_sat || actual = "Timeout" || actual = "GaveUp"
  else
    match actual with
    | "Theorem" | "Unsatisfiable" | "ContradictoryAxioms"
    | "Satisfiable" | "CounterSatisfiable" -> true
    | _ -> false

let is_timeout status = status = "Timeout"
let is_error status = status = "Error" || status = "InputError" || status = "ResourceOut"

type bucket = {
  mutable total : int;
  mutable solved : int;
  mutable timeout : int;
  mutable error : int;
  mutable regressions : int;
  mutable new_solved : int;
  mutable axiom_selection : int;
  mutable near_timeout : int;
}

let empty_bucket () =
  {
    total = 0;
    solved = 0;
    timeout = 0;
    error = 0;
    regressions = 0;
    new_solved = 0;
    axiom_selection = 0;
    near_timeout = 0;
  }

let bucket tbl key =
  match Hashtbl.find_opt tbl key with
  | Some b -> b
  | None ->
      let b = empty_bucket () in
      Hashtbl.add tbl key b;
      b

let sorted_keys tbl =
  Hashtbl.fold (fun k _ acc -> k :: acc) tbl [] |> List.sort String.compare

let print_bucket name b =
  Printf.printf
    "  %-18s total=%4d solved=%4d timeout=%4d error=%3d regressions=%3d new=%3d axiom_selection=%4d near_timeout=%3d\n"
    name
    b.total
    b.solved
    b.timeout
    b.error
    b.regressions
    b.new_solved
    b.axiom_selection
    b.near_timeout

let usage () =
  prerr_endline
    "Usage: analyze_profiled_bench --base BASE.csv --new NEW.csv [--near-timeout-ratio R] [--list-limit N]";
  exit 2

let parse_args () =
  let base = ref None in
  let new_csv = ref None in
  let near_timeout_ratio = ref 0.95 in
  let list_limit = ref 30 in
  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--base" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          base := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--new" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          new_csv := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--near-timeout-ratio" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          near_timeout_ratio := float_of_string Sys.argv.(i + 1);
          loop (i + 2)
      | "--list-limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          list_limit := int_of_string Sys.argv.(i + 1);
          loop (i + 2)
      | _ -> usage ()
  in
  loop 1;
  match !base, !new_csv with
  | Some base, Some new_csv -> base, new_csv, !near_timeout_ratio, !list_limit
  | _ -> usage ()

let take n xs =
  let rec aux acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: tl -> aux (x :: acc) (n - 1) tl
  in
  aux [] n xs

let print_rows title rows limit =
  Printf.printf "\n%s (%d shown / %d total):\n" title (min limit (List.length rows)) (List.length rows);
  rows
  |> take limit
  |> List.iter
       (fun (old_r, new_r) ->
         Printf.printf
           "  %s profile=%s old=%s new=%s time=%.3fs axiom_selection=%s\n"
           new_r.problem
           new_r.ip_profile
           old_r.ip_status
           new_r.ip_status
           new_r.ip_time_s
           (match new_r.ip_axiom_selection with
            | Some b -> string_of_bool b
            | None -> ""))

let () =
  let base_path, new_path, near_timeout_ratio, list_limit = parse_args () in
  let base = read_bench base_path in
  let new_b = read_bench new_path in
  let base_map = row_map base.rows in
  let by_profile = Hashtbl.create 16 in
  let by_axiom = Hashtbl.create 4 in
  let global = empty_bucket () in
  let regressions = ref [] in
  let improvements = ref [] in
  let common = ref 0 in
  let timeout_limit =
    new_b.rows
    |> List.fold_left (fun acc r -> max acc r.ip_time_s) 0.0
  in
  let near_timeout_s = timeout_limit *. near_timeout_ratio in
  List.iter
    (fun new_r ->
      let profile =
        if new_r.ip_profile = "" then "unknown" else new_r.ip_profile
      in
      let profile_b = bucket by_profile profile in
      let axiom_key =
        match new_r.ip_axiom_selection with
        | Some true -> "axiom-selection=true"
        | Some false -> "axiom-selection=false"
        | None -> "axiom-selection=unknown"
      in
      let axiom_b = bucket by_axiom axiom_key in
      let buckets = [ global; profile_b; axiom_b ] in
      List.iter (fun b -> b.total <- b.total + 1) buckets;
      if is_ip_success new_r.expected new_r.ip_status then
        List.iter (fun b -> b.solved <- b.solved + 1) buckets;
      if is_timeout new_r.ip_status then
        List.iter (fun b -> b.timeout <- b.timeout + 1) buckets;
      if is_error new_r.ip_status then
        List.iter (fun b -> b.error <- b.error + 1) buckets;
      if new_r.ip_axiom_selection = Some true then
        List.iter (fun b -> b.axiom_selection <- b.axiom_selection + 1) buckets;
      if new_r.ip_time_s >= near_timeout_s && near_timeout_s > 0.0 then
        List.iter (fun b -> b.near_timeout <- b.near_timeout + 1) buckets;
      match Hashtbl.find_opt base_map new_r.problem with
      | None -> ()
      | Some old_r ->
          incr common;
          let old_ok = is_ip_success old_r.expected old_r.ip_status in
          let new_ok = is_ip_success new_r.expected new_r.ip_status in
          if old_ok && not new_ok then begin
            regressions := (old_r, new_r) :: !regressions;
            List.iter (fun b -> b.regressions <- b.regressions + 1) buckets
          end else if (not old_ok) && new_ok then begin
            improvements := (old_r, new_r) :: !improvements;
            List.iter (fun b -> b.new_solved <- b.new_solved + 1) buckets
          end)
    new_b.rows;
  let regressions =
    List.rev !regressions
    |> List.sort (fun (_, a) (_, b) ->
         compare (a.ip_profile, a.problem) (b.ip_profile, b.problem))
  in
  let improvements =
    List.rev !improvements
    |> List.sort (fun (_, a) (_, b) ->
         compare (a.ip_profile, a.problem) (b.ip_profile, b.problem))
  in
  Printf.printf "Base: %s (%s)\n" base.file base.date;
  Printf.printf "New : %s (%s)\n" new_b.file new_b.date;
  Printf.printf "Common problems: %d\n" !common;
  Printf.printf "Regressions: %d\n" (List.length regressions);
  Printf.printf "New solved: %d\n" (List.length improvements);
  Printf.printf "Near-timeout threshold: %.3fs\n\n" near_timeout_s;
  Printf.printf "Global:\n";
  print_bucket "all" global;
  Printf.printf "\nBy profile:\n";
  sorted_keys by_profile
  |> List.iter (fun k -> print_bucket k (Hashtbl.find by_profile k));
  Printf.printf "\nBy axiom selection:\n";
  sorted_keys by_axiom
  |> List.iter (fun k -> print_bucket k (Hashtbl.find by_axiom k));
  print_rows "Regressions" regressions list_limit;
  print_rows "New solved" improvements list_limit
