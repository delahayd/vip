open Prover_lib

type profile =
  | Equality_heavy
  | Equality_light
  | Non_equality
  | Large_general

let string_of_profile = function
  | Equality_heavy -> "equality-heavy"
  | Equality_light -> "equality-light"
  | Non_equality -> "non-equality"
  | Large_general -> "large-general"

let starts_with s prefix =
  let ls = String.length s in
  let lp = String.length prefix in
  ls >= lp && String.sub s 0 lp = prefix

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

let is_problem_file path =
  Filename.check_suffix path ".p"

let rec walk dir =
  let entries =
    Sys.readdir dir
    |> Array.to_list
    |> List.sort String.compare
  in
  List.concat
    (List.map
       (fun name ->
         let path = Filename.concat dir name in
         if Sys.is_directory path then walk path
         else if is_problem_file path then [ path ]
         else [])
       entries)

let normalize_dir d =
  let len = String.length d in
  if len > 1 && d.[len - 1] = Filename.dir_sep.[0] then
    String.sub d 0 (len - 1)
  else
    d

let relative_to_dir ~dir path =
  let dir = normalize_dir dir in
  let prefix = dir ^ Filename.dir_sep in
  if path = dir then Filename.basename path
  else if starts_with path prefix then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else
    path

let literal_contains_equality = function
  | Types.Pos { pred = "="; args = [ _; _ ] }
  | Types.Neg { pred = "="; args = [ _; _ ] } -> true
  | _ -> false

let literal_is_negative = function
  | Types.Neg _ -> true
  | Types.Pos _ -> false

let rec term_size = function
  | Types.Var _ -> 1
  | Types.Fun (_, args) ->
      1 + List.fold_left (fun acc t -> acc + term_size t) 0 args

let literal_term_size = function
  | Types.Pos a | Types.Neg a ->
      List.fold_left (fun acc t -> acc + term_size t) 1 a.args

let ratio n d =
  if d = 0 then 0.0 else float_of_int n /. float_of_int d

let clause_stats clauses =
  let clause_count = List.length clauses in
  let total_literals =
    List.fold_left (fun acc c -> acc + List.length c) 0 clauses
  in
  let equality_literals =
    List.fold_left
      (fun acc c ->
        acc
        + List.fold_left
            (fun n lit -> if literal_contains_equality lit then n + 1 else n)
            0
            c)
      0
      clauses
  in
  let negative_literals =
    List.fold_left
      (fun acc c ->
        acc
        + List.fold_left
            (fun n lit -> if literal_is_negative lit then n + 1 else n)
            0
            c)
      0
      clauses
  in
  let unit_clauses =
    List.fold_left (fun acc c -> if List.length c = 1 then acc + 1 else acc) 0 clauses
  in
  let total_term_size =
    List.fold_left
      (fun acc c ->
        acc + List.fold_left (fun n lit -> n + literal_term_size lit) 0 c)
      0
      clauses
  in
  ( clause_count,
    total_literals,
    equality_literals,
    ratio equality_literals total_literals,
    ratio total_term_size total_literals,
    ratio unit_clauses clause_count,
    ratio negative_literals total_literals )

let classify ~clause_count ~equality_literals ~equality_ratio ~avg_term =
  let min_eq_literals =
    match Sys.getenv_opt "IP_PROFILE_EQ_HEAVY_MIN_LITERALS" with
    | Some s -> (try max 0 (int_of_string s) with Failure _ -> 4)
    | None -> 4
  in
  let min_eq_ratio =
    match Sys.getenv_opt "IP_PROFILE_EQ_HEAVY_MIN_RATIO" with
    | Some s -> (try float_of_string s with Failure _ -> 0.12)
    | None -> 0.12
  in
  let min_avg_term =
    match Sys.getenv_opt "IP_PROFILE_EQ_HEAVY_MIN_AVG_TERM" with
    | Some s -> (try float_of_string s with Failure _ -> 3.5)
    | None -> 3.5
  in
  let large_min =
    match Sys.getenv_opt "IP_PROFILE_LARGE_MIN_CLAUSES" with
    | Some s -> (try max 0 (int_of_string s) with Failure _ -> 500)
    | None -> 500
  in
  if equality_literals >= min_eq_literals
     && equality_ratio >= min_eq_ratio
     && avg_term >= min_avg_term
  then
    Equality_heavy
  else if equality_literals > 0 then
    Equality_light
  else if clause_count >= large_min then
    Large_general
  else
    Non_equality

let usage () =
  prerr_endline
    "Usage: profile_problems --dir DIR [--tptp DIR] [--out FILE] [--limit N]";
  exit 2

let parse_args () =
  let dir = ref None in
  let tptp = ref None in
  let out = ref None in
  let limit = ref None in
  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--dir" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          dir := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--tptp" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          tptp := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--out" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          out := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          limit := Some (int_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | _ -> usage ()
  in
  loop 1;
  match !dir with
  | None -> usage ()
  | Some dir -> dir, !tptp, !out, !limit

let take limit xs =
  match limit with
  | None -> xs
  | Some n ->
      let rec aux acc n = function
        | [] -> List.rev acc
        | _ when n <= 0 -> List.rev acc
        | x :: tl -> aux (x :: acc) (n - 1) tl
      in
      aux [] n xs

let () =
  let dir, tptp, out, limit = parse_args () in
  let load_config =
    match tptp with
    | None -> Tptp_frontend.default_load_config
    | Some d -> { Tptp_frontend.include_paths = [ d ]; use_tptp_env = true }
  in
  let files = walk dir |> take limit in
  let oc, close =
    match out with
    | None -> stdout, (fun () -> ())
    | Some path -> let oc = open_out path in oc, fun () -> close_out oc
  in
  Printf.fprintf oc
    "problem,profile,clauses,literals,equality_literals,equality_ratio,avg_term,unit_ratio,negative_ratio,status\n";
  List.iter
    (fun file ->
      Clausify.reset_fresh_state ();
      Clause.reset_fresh_counter ();
      try
        let parsed = Tptp_frontend.load_problem ~config:load_config file in
        let part = Clausify.partition_input_clauses parsed.inputs in
        let clauses = part.axioms @ part.support in
        let clause_count, literal_count, equality_literals, equality_ratio,
            avg_term, unit_ratio, negative_ratio =
          clause_stats clauses
        in
        let profile =
          classify ~clause_count ~equality_literals ~equality_ratio ~avg_term
        in
        Printf.fprintf oc "%s,%s,%d,%d,%d,%.6f,%.6f,%.6f,%.6f,ok\n"
          (csv_escape (relative_to_dir ~dir file))
          (csv_escape (string_of_profile profile))
          clause_count
          literal_count
          equality_literals
          equality_ratio
          avg_term
          unit_ratio
          negative_ratio
      with exn ->
        Printf.fprintf oc "%s,%s,%d,%d,%d,%.6f,%.6f,%.6f,%.6f,%s\n"
          (csv_escape (relative_to_dir ~dir file))
          (csv_escape "error")
          0 0 0 0.0 0.0 0.0 0.0
          (csv_escape (Printexc.to_string exn)))
    files;
  close ()
