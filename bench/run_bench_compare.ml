type prover = {
  name : string;
  cmd : string -> int -> string;
}

type result = {
  status : string;
  raw : string;
}

type config = {
  dir : string;
  timeout : int;
  debug : bool;
}

let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in
  aux 0

let provers = [
  {
    name = "ip";
    cmd = (fun file timeout ->
      Printf.sprintf "./bin/ip --time-limit %d %s" timeout file);
  };
  {
    name = "vampire";
    cmd = (fun file timeout ->
      Printf.sprintf "vampire --mode casc --time_limit %d %s" timeout file);
  };
  {
    name = "e";
    cmd = (fun file timeout ->
      Printf.sprintf "eprover --auto --cpu-limit=%d %s" timeout file);
  };
  {
    name = "zenon";
    cmd = (fun file timeout ->
      Printf.sprintf "zenon -itptp -max-time %d %s" timeout file);
  };
]

let usage () =
  prerr_endline "Usage: run_bench [--debug] <directory> <timeout_seconds>";
  exit 2

let parse_args () =
  let debug = ref false in
  let positional = ref [] in

  Array.iteri
    (fun i arg ->
      if i > 0 then
        match arg with
        | "--debug" ->
            debug := true
        | s ->
            positional := !positional @ [ s ])
    Sys.argv;

  match !positional with
  | [ dir; timeout_s ] ->
      let timeout =
        try int_of_string timeout_s
        with Failure _ -> usage ()
      in
      { dir; timeout; debug = !debug }
  | _ ->
      usage ()

let is_problem_file f =
  Filename.check_suffix f ".p"

let rec walk dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare
  |> List.concat_map (fun name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then walk path
      else if is_problem_file path then [path]
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
  (output, status)

let exit_code_of_status = function
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n -> 128 + n
  | Unix.WSTOPPED n -> 128 + n

let extract_status output =
  let lines = String.split_on_char '\n' output in

  let find_szs () =
    List.find_opt
      (fun l ->
        String.length l >= 13 &&
        String.sub l 0 13 = "% SZS status ")
      lines
  in

  match find_szs () with
  | Some l ->
      begin
        match String.split_on_char ' ' l with
        | _ :: "SZS" :: "status" :: status :: _ -> status
        | _ -> "Unknown"
      end

  | None ->
      let lower = String.lowercase_ascii output in

      if contains_substring output "(* PROOF-FOUND *)" then
        "Unsatisfiable"
      else if contains_substring lower "timeout" then
        "Timeout"
      else if contains_substring lower "time limit" then
        "Timeout"
      else
        "Unknown"

let run_prover config prover file =
  let cmd = prover.cmd file config.timeout in
  let output, status = run_command cmd in
  let code = exit_code_of_status status in
  let lower = String.lowercase_ascii output in

  let status_str =
    if code <> 0 then
      if contains_substring lower "timeout"
         || contains_substring lower "time limit"
      then
        "Timeout"
      else
        "Error"
    else
      extract_status output
  in

  if config.debug && status_str = "Error" then
    Printf.eprintf
      "\n[DEBUG] Error on %s with %s\nCommand: %s\nExit code: %d\nOutput:\n%s\n%!"
      file
      prover.name
      cmd
      code
      output;

  {
    status = status_str;
    raw = output;
  }

let print_header () =
  Printf.printf "%-60s | %-14s | %-14s | %-14s | %-14s\n%!"
    "problem" "ip" "vampire" "e" "zenon";
  Printf.printf "%s\n%!" (String.make 126 '-')

let print_row file results =
  let get name =
    match List.assoc_opt name results with
    | Some r -> r.status
    | None -> "Missing"
  in
  Printf.printf "%-60s | %-14s | %-14s | %-14s | %-14s\n%!"
    file
    (get "ip")
    (get "vampire")
    (get "e")
    (get "zenon")

let () =
  let config = parse_args () in
  let files = walk config.dir in

  print_header ();

  List.iter
    (fun file ->
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
                { status = "Error"; raw = "" }
            in
            prover.name, r)
          provers
      in
      print_row file results)
    files
