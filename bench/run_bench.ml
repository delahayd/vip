open Prover_lib
open Prover

type bench_config = {
  dir : string;
  time_limit_s : float option;
  max_clauses : int option;
  mode : Resolution.inference_mode;
}

type summary = {
  mutable total : int;
  mutable unsat : int;
  mutable gave_up : int;
  mutable timeout : int;
  mutable resource_out : int;
  mutable input_error : int;
  mutable other : int;
}

let default_dir = "tests/integration"

let parse_mode = function
  | "unrestricted" -> Resolution.Unrestricted
  | "ordered" -> Resolution.Ordered
  | "ordered-fallback" -> Resolution.Ordered_with_fallback
  | s -> failwith ("Unknown mode: " ^ s)

let string_of_mode = function
  | Resolution.Unrestricted -> "unrestricted"
  | Resolution.Ordered -> "ordered"
  | Resolution.Ordered_with_fallback -> "ordered-fallback"

let parse_args () =
  let dir = ref default_dir in
  let time_limit_s = ref None in
  let max_clauses = ref None in
  let mode = ref Resolution.Ordered_with_fallback in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--dir" ->
          if i + 1 >= Array.length Sys.argv then failwith "--dir requires a value";
          dir := Sys.argv.(i + 1);
          loop (i + 2)
      | "--time-limit" ->
          if i + 1 >= Array.length Sys.argv then failwith "--time-limit requires a value";
          time_limit_s := Some (float_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--max-clauses" ->
          if i + 1 >= Array.length Sys.argv then failwith "--max-clauses requires a value";
          max_clauses := Some (int_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--mode" ->
          if i + 1 >= Array.length Sys.argv then failwith "--mode requires a value";
          mode := parse_mode Sys.argv.(i + 1);
          loop (i + 2)
      | s ->
          failwith ("Unknown argument: " ^ s)
  in
  loop 1;

  {
    dir = !dir;
    time_limit_s = !time_limit_s;
    max_clauses = !max_clauses;
    mode = !mode;
  }

let make_prover_config config =
  {
    default_config with
    time_limit_s = config.time_limit_s;
    max_generated_clauses = config.max_clauses;
    inference_mode = config.mode;
  }

let update_summary summary status =
  summary.total <- summary.total + 1;
  match status with
  | Unsatisfiable | Theorem ->
      summary.unsat <- summary.unsat + 1
  | GaveUp ->
      summary.gave_up <- summary.gave_up + 1
  | Timeout ->
      summary.timeout <- summary.timeout + 1
  | ResourceOut ->
      summary.resource_out <- summary.resource_out + 1
  | InputError ->
      summary.input_error <- summary.input_error + 1
  | Satisfiable | CounterSatisfiable ->
      summary.other <- summary.other + 1

let run_one config summary file =
  let prover_config = make_prover_config config in
  try
    let outcome = run_file ~config:prover_config file in
    update_summary summary outcome.status;
    Printf.printf "%-70s %s\n%!"
      file
      (string_of_szs_status outcome.status)
  with
  | Tptp_frontend.Error msg ->
      update_summary summary InputError;
      Printf.printf "%-70s InputError %s\n%!" file msg
  | Failure msg ->
      update_summary summary InputError;
      Printf.printf "%-70s InputError %s\n%!" file msg
  | exn ->
      update_summary summary InputError;
      Printf.printf "%-70s InputError %s\n%!" file (Printexc.to_string exn)

let is_problem_file path =
  Filename.check_suffix path ".p"

let rec walk_and_run config summary dir =
  let entries =
    try Sys.readdir dir
    with Sys_error msg ->
      Printf.eprintf "Cannot read directory %s: %s\n%!" dir msg;
      [||]
  in
  Array.sort String.compare entries;
  Array.iter
    (fun name ->
      let path = Filename.concat dir name in
      try
        if Sys.is_directory path then
          walk_and_run config summary path
        else if is_problem_file path then
          run_one config summary path
      with Sys_error msg ->
        Printf.eprintf "Skipping %s: %s\n%!" path msg)
    entries

let print_summary summary =
  print_endline "\nSummary:";
  Printf.printf "%-20s %d\n" "total" summary.total;
  Printf.printf "%-20s %d\n" "unsat/theorem" summary.unsat;
  Printf.printf "%-20s %d\n" "gave_up" summary.gave_up;
  Printf.printf "%-20s %d\n" "timeout" summary.timeout;
  Printf.printf "%-20s %d\n" "resource_out" summary.resource_out;
  Printf.printf "%-20s %d\n" "input_error" summary.input_error;
  Printf.printf "%-20s %d\n" "other" summary.other

let () =
  let config = parse_args () in
  let summary = {
    total = 0;
    unsat = 0;
    gave_up = 0;
    timeout = 0;
    resource_out = 0;
    input_error = 0;
    other = 0;
  } in

  Printf.printf "Running problems from %s (mode=%s)\n%!"
    config.dir
    (string_of_mode config.mode);

  walk_and_run config summary config.dir;
  print_summary summary
