let version_n = 1

exception Invalid_version_index of int

let version_of_n n =
  if n < 1 then raise (Invalid_version_index n)
  else exp 1.0 -. (1.0 /. float_of_int n)

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

let command_available cmd =
  Sys.command
    (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote cmd))
  = 0

let check_duke_dependencies duke =
  if duke && not (command_available "ffmpeg") then begin
    Printf.eprintf
      "Warning: ffmpeg is not installed or not available in PATH. Sounds for --duke mode will not be played.\n%!";
    false
  end else
    duke

let find_mp3 mp3_file =
  let candidates =
    List.map
      (fun dir -> Filename.concat dir mp3_file)
      Sites.Sites.sounds
  in
  match List.find_opt Sys.file_exists candidates with
  | Some file -> file
  | None -> mp3_file

let play_mp3 filename =
  let mp3_file = find_mp3 filename in
  let quoted = Filename.quote mp3_file in

  let cmd =
    Printf.sprintf
      "ffmpeg -loglevel error -i %s -f s16le -acodec pcm_s16le -ac 2 -ar 44100 - | aplay -q -t raw -f S16_LE -c 2 -r 44100"
      quoted
  in

  match Sys.command cmd with
  | 0 -> ()
  | n ->
      Printf.eprintf "Error: MP3 playback failed with the code %d\n" n;
      exit n

let play_mp3_safely mp3_file =
  try
    flush stdout;
    flush stderr;
    match Unix.fork () with
    | 0 ->
        begin
          try
            play_mp3 mp3_file;
            Unix._exit 0
          with _ ->
            Unix._exit 0
        end
    | _pid ->
        ()
  with _ ->
    ()

let play_duke_safely () =
  play_mp3_safely "unsat.mp3"

let play_timeout_safely () =
  play_mp3_safely "timeout.mp3"

let usage () =
  prerr_endline
    "Usage: ip [--version] [--duke] [--proof] [--proof-format none|internal|tstp] [--proof-tstp] [--competition-output] [--time-limit SECONDS] [--max-clauses N] [--mode MODE] [--portfolio legacy-modern|modern-legacy|legacy-only|modern-only|modern-compat-only|scheduled|feq-modern|experimental-casc|casc-aggressive|casc-240|casc-feq-probe] [--tptp DIR] [--sos|--no-sos] FILE";
  exit 2

type proof_format =
  | No_proof
  | Internal
  | Tstp

let mode_of_string = function
  | "unrestricted" -> Prover_lib.Resolution.Unrestricted
  | "ordered" -> Prover_lib.Resolution.Ordered
  | "ordered-fallback" -> Prover_lib.Resolution.Ordered_with_fallback
  | s ->
      prerr_endline ("Unknown mode: " ^ s);
      usage ()

let portfolio_of_string = function
  | "legacy-modern" | "portfolio" -> Prover_lib.Prover.Legacy_then_modern
  | "scheduled" | "scheduler" -> Prover_lib.Prover.Scheduled_portfolio
  | "modern-legacy" | "modern-first" -> Prover_lib.Prover.Modern_then_legacy
  | "legacy-only" | "legacy" -> Prover_lib.Prover.Legacy_only
  | "modern-only" | "modern" -> Prover_lib.Prover.Modern_only
  | "modern-compat-only" | "compat-only" | "compat" ->
      Prover_lib.Prover.Modern_compat_only
  | "feq-modern" | "feq" | "equality" ->
      Prover_lib.Prover.Feq_modern
  | "experimental-casc" | "casc-experimental" | "exp-casc" ->
      Prover_lib.Prover.Experimental_casc
  | "casc-aggressive" | "aggressive-casc" | "aggressive" ->
      Prover_lib.Prover.Casc_aggressive
  | "casc-240" | "casc240" | "casc-final" | "final-casc" ->
      Prover_lib.Prover.Casc_240
  | "casc-feq-probe" | "feq-probe" | "casc-equality-probe" ->
      Prover_lib.Prover.Casc_feq_probe
  | s ->
      prerr_endline ("Unknown portfolio mode: " ^ s);
      usage ()

let proof_format_of_string = function
  | "none" | "off" | "false" -> No_proof
  | "internal" | "trace" -> Internal
  | "tstp" -> Tstp
  | s ->
      prerr_endline ("Unknown proof format: " ^ s);
      usage ()

let parse_args () =
  let file = ref None in
  let proof_format = ref No_proof in
  let time_limit_s = ref None in
  let max_generated_clauses = ref None in
  let inference_mode = ref Prover_lib.Resolution.Ordered_with_fallback in
  let show_version = ref false in
  let duke = ref false in
  let tptp_dir = ref None in
  let use_sos = ref true in
  let portfolio_mode = ref Prover_lib.Prover.Legacy_then_modern in
  let competition_output = ref false in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--version" ->
          show_version := true;
          loop (i + 1)

      | "--sos" ->
          use_sos := true;
          loop (i + 1)

      | "--no-sos" ->
          use_sos := false;
          loop (i + 1)

      | "--duke" ->
          duke := true;
          loop (i + 1)

      | "--proof" ->
          proof_format := Internal;
          loop (i + 1)

      | "--proof-tstp" ->
          proof_format := Tstp;
          loop (i + 1)

      | "--proof-format" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          proof_format := proof_format_of_string Sys.argv.(i + 1);
          loop (i + 2)

      | "--competition-output" | "--casc-output" ->
          competition_output := true;
          proof_format := Tstp;
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

      | "--portfolio" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          portfolio_mode := portfolio_of_string Sys.argv.(i + 1);
          loop (i + 2)

      | "--tptp" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          tptp_dir := Some Sys.argv.(i + 1);
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
              print_derivation = !proof_format = Internal;
              inference_mode = !inference_mode;
              tptp_dir = !tptp_dir;
              use_sos = !use_sos;
              portfolio_mode = !portfolio_mode;
            },
            !duke,
            !proof_format,
            !competition_output )

let () =
  match parse_args () with
  | `Version ->
      print_version ()

  | `Run (filename, config, duke, proof_format, competition_output) ->
      let duke = check_duke_dependencies duke in
      try
        let outcome = Prover_lib.Prover.run_file ~config filename in
        Prover_lib.Prover.print_szs ~verbose:(not competition_output) outcome;

        if duke then begin
          match outcome.Prover_lib.Prover.status with
          | Prover_lib.Prover.Unsatisfiable -> play_duke_safely ()
          | Prover_lib.Prover.Timeout -> play_timeout_safely ()
          | _ -> ()
        end;

        begin
          match proof_format with
          | No_proof -> ()
          | Internal ->
              print_endline "% Proof trace:";
              Prover_lib.Resolution.print_derivation outcome.derivation
          | Tstp ->
              begin
                match outcome.Prover_lib.Prover.empty_clause with
                | Some _ ->
                    Prover_lib.Resolution.print_tstp_derivation
                      ~problem:filename
                      ?prelude:outcome.Prover_lib.Prover.tstp_prelude
                      ~skip_initial:(outcome.Prover_lib.Prover.tstp_prelude <> None)
                      outcome.derivation
                | None ->
                    if not competition_output then
                      print_endline "% No refutation proof available."
              end
        end
      with
      | Prover_lib.Resolution.Timeout_hit ->
	  Printf.printf "%% SZS status Timeout for %s\n%!" filename;
	  if duke then play_timeout_safely ()
      | exn ->
          Printf.printf "%% SZS status InputError for %s\n%!" filename;
          if not competition_output then
            Printf.printf "%% InputError %s\n%!" (Printexc.to_string exn);
          exit 1
