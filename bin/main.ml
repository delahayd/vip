open Prover_lib
open Prover

let usage () =
  prerr_endline
    "Usage: prover_cli [--time-limit SEC] [--max-clauses N] [--proof] [--mode unrestricted|ordered|ordered-fallback] <fichier.p>";
  exit 2

let parse_mode = function
  | "unrestricted" -> Resolution.Unrestricted
  | "ordered" -> Resolution.Ordered
  | "ordered-fallback" -> Resolution.Ordered_with_fallback
  | s ->
      prerr_endline ("Unknown mode: " ^ s);
      usage ()

let parse_args () =
  let time_limit_s = ref None in
  let max_generated_clauses = ref None in
  let proof = ref false in
  let inference_mode = ref Resolution.Ordered_with_fallback in
  let file = ref None in

  let rec loop i =
    if i >= Array.length Sys.argv then
      ()
    else
      match Sys.argv.(i) with
      | "--time-limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          time_limit_s := Some (float_of_string Sys.argv.(i + 1));
          loop (i + 2)

      | "--max-clauses" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          max_generated_clauses := Some (int_of_string Sys.argv.(i + 1));
          loop (i + 2)

      | "--proof" ->
          proof := true;
          loop (i + 1)

      | "--mode" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          inference_mode := parse_mode Sys.argv.(i + 1);
          loop (i + 2)

      | s when String.length s > 0 && s.[0] = '-' ->
          prerr_endline ("Unknown option: " ^ s);
          usage ()

      | s ->
          begin
            match !file with
            | None ->
                file := Some s;
                loop (i + 1)
            | Some _ ->
                usage ()
          end
  in

  loop 1;

  match !file with
  | None -> usage ()
  | Some f ->
      ({
         time_limit_s = !time_limit_s;
         max_generated_clauses = !max_generated_clauses;
         print_derivation = !proof;
         inference_mode = !inference_mode;
       }, f)

let exit_code_of_status = function
  | Theorem | Unsatisfiable -> 0
  | Timeout -> 124
  | ResourceOut -> 3
  | GaveUp -> 1
  | Satisfiable | CounterSatisfiable -> 10
  | InputError -> 2

let input_error_outcome filename =
  {
    status = InputError;
    info = {
      file = Some filename;
      clause_count = 0;
      generated_clause_count = 0;
    };
    derivation = [];
    empty_clause = None;
    resolution_stats = None;
  }

let () =
  let config, filename = parse_args () in
  try
    let outcome = run_file ~config filename in
    print_szs outcome;

    if config.print_derivation then begin
      print_endline "% SZS output start CNFRefutation";
      Resolution.print_derivation outcome.derivation;
      print_endline "% SZS output end CNFRefutation"
    end;

    exit (exit_code_of_status outcome.status)
  with
  | Tptp_frontend.Error msg ->
      let outcome = input_error_outcome filename in
      print_szs outcome;
      prerr_endline ("% message: " ^ msg);
      exit (exit_code_of_status InputError)

  | Failure msg ->
      let outcome = input_error_outcome filename in
      print_szs outcome;
      prerr_endline ("% message: " ^ msg);
      exit (exit_code_of_status InputError)
