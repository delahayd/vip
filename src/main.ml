open Prover_lib

let version_n = 1

exception Invalid_version_index of int

let version_of_n n =
  if n < 1 then
    raise (Invalid_version_index n)
  else
    exp 1.0 -. (1.0 /. float_of_int n)

let version_string () =
  Printf.sprintf "%.12f" (version_of_n version_n)

let print_header () =
  Header.header
  |> String.to_seq
  |> Seq.drop 1
  |> String.of_seq
  |> print_string

let print_version () =
  print_header ();
  Printf.printf "ip version %s (seed n = %d)\n" (version_string ()) version_n

let usage () =
  prerr_endline
    "Usage: ip [--version] [--proof] [--time-limit SECONDS] [--max-clauses N] [--mode MODE] FILE";
  exit 2

let mode_of_string = function
  | "unrestricted" -> Prover_lib.Resolution.Unrestricted
  | "ordered" -> Prover_lib.Resolution.Ordered
  | "ordered-fallback" -> Prover_lib.Resolution.Ordered_with_fallback
  | s ->
      prerr_endline ("Unknown mode: " ^ s);
      usage ()

let parse_args () =
  let file = ref None in
  let print_derivation = ref false in
  let time_limit_s = ref None in
  let max_generated_clauses = ref None in
  let inference_mode = ref Prover_lib.Resolution.Ordered_with_fallback in
  let show_version = ref false in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--version" ->
          show_version := true;
          loop (i + 1)

      | "--proof" ->
          print_derivation := true;
          loop (i + 1)

      | "--time-limit" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          let t =
            try float_of_string Sys.argv.(i + 1)
            with Failure _ -> usage ()
          in
          time_limit_s := Some t;
          loop (i + 2)

      | "--max-clauses" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          let n =
            try int_of_string Sys.argv.(i + 1)
            with Failure _ -> usage ()
          in
          max_generated_clauses := Some n;
          loop (i + 2)

      | "--mode" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          inference_mode := mode_of_string Sys.argv.(i + 1);
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
                prerr_endline "Too many input files.";
                usage ()
          end
  in

  loop 1;

  if !show_version then
    `Version
  else
    match !file with
    | None -> usage ()
    | Some filename ->
        `Run
          ( filename,
            {
              Prover_lib.Prover.time_limit_s = !time_limit_s;
              max_generated_clauses = !max_generated_clauses;
              print_derivation = !print_derivation;
              inference_mode = !inference_mode;
            } )

let () =
  match parse_args () with
  | `Version ->
      print_version ()

  | `Run (filename, config) ->
      try
        let outcome = Prover_lib.Prover.run_file ~config filename in
        Prover_lib.Prover.print_szs outcome;

        if config.Prover_lib.Prover.print_derivation then begin
          print_endline "% Proof trace:";
          Prover_lib.Resolution.print_derivation outcome.derivation
        end
      with
      | Prover_lib.Resolution.Timeout_hit ->
          Printf.printf "%% SZS status Timeout for %s\n" filename
      | exn ->
          Printf.eprintf "InputError %s\n" (Printexc.to_string exn);
          exit 1
