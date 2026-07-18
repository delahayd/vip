open Fof

exception Error of string

type parsed_problem = {
  file : string option;
  inputs : annotated_input list;
}

type load_config = {
  include_paths : string list;
  use_tptp_env : bool;
}

let default_load_config = {
  include_paths = [];
  use_tptp_env = true;
}

module StringSet = Set.Make (String)

let validate_user_symbol kind symbol =
  if String.length symbol > 0 && symbol.[0] = '$' then
    raise
      (Error
         (Printf.sprintf
            "Symbole TPTP défini non supporté utilisé comme %s: %s"
            kind symbol))

let rec validate_term = function
  | Types.Var _ -> ()
  | Types.Fun (symbol, args) ->
      validate_user_symbol "fonction" symbol;
      List.iter validate_term args

let validate_atom atom =
  if atom.Types.pred <> "=" then
    validate_user_symbol "prédicat" atom.Types.pred;
  List.iter validate_term atom.Types.args

let validate_literal = function
  | Types.Pos atom | Types.Neg atom ->
      if atom.Types.pred <> "$true" && atom.Types.pred <> "$false" then
        validate_atom atom

let rec validate_formula = function
  | FTrue | FFalse -> ()
  | Atom atom -> validate_atom atom
  | Not formula -> validate_formula formula
  | And (left, right)
  | Or (left, right)
  | Imp (left, right)
  | RevImp (left, right)
  | Iff (left, right)
  | Xor (left, right) ->
      validate_formula left;
      validate_formula right
  | Forall (_, formula) | Exists (_, formula) -> validate_formula formula

let validate_input = function
  | Input_cnf { clause; _ } -> List.iter validate_literal clause
  | Input_fof { formula; _ } -> validate_formula formula
  | Input_include _ -> ()

let validate_inputs inputs =
  List.iter validate_input inputs;
  inputs

let parse_lexbuf lexbuf =
  try
    Tptp_parser.file Tptp_lexer.token lexbuf |> validate_inputs
  with
  | Tptp_lexer.Lexing_error msg ->
      raise (Error msg)
  | Tptp_parser.Error ->
      let p = lexbuf.Lexing.lex_curr_p in
      let col = p.pos_cnum - p.pos_bol + 1 in
      raise (Error (Printf.sprintf "Erreur de parsing ligne %d, colonne %d" p.pos_lnum col))
  | Failure msg ->
      raise (Error msg)

let parse_string s =
  parse_lexbuf (Lexing.from_string s)

let parse_channel ic =
  parse_lexbuf (Lexing.from_channel ic)

let parse_file filename =
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> parse_channel ic)

let input_name = function
  | Input_cnf { name; _ } -> Some name
  | Input_fof { name; _ } -> Some name
  | Input_include _ -> None

let filter_by_name only xs =
  match only with
  | None -> xs
  | Some names ->
      List.filter
        (fun x ->
          match input_name x with
          | None -> false
          | Some n -> List.mem n names)
        xs

let set_source_file filename = function
  | Input_cnf r -> Input_cnf { r with source_file = Some filename }
  | Input_fof r -> Input_fof { r with source_file = Some filename }
  | Input_include _ as input -> input

let is_file path =
  try Sys.file_exists path && not (Sys.is_directory path)
  with Sys_error _ -> false

let normalize_path path =
  if Filename.is_relative path then
    Filename.concat (Sys.getcwd ()) path
  else
    path

let getenv_opt name =
  try Some (Sys.getenv name) with Not_found -> None

let tptp_env_candidates () =
  match getenv_opt "TPTP" with
  | None -> []
  | Some root ->
      let root = normalize_path root in
      [root]

let candidate_include_dirs ~config ~from_file =
  let current_dir = Filename.dirname from_file in
  let env_dirs =
    if config.use_tptp_env then tptp_env_candidates () else []
  in
  current_dir :: (env_dirs @ config.include_paths)

let dedup_preserve_order xs =
  let rec aux seen acc = function
    | [] -> List.rev acc
    | x :: tl ->
        if List.mem x seen then
          aux seen acc tl
        else
          aux (x :: seen) (x :: acc) tl
  in
  aux [] [] xs

let resolve_include_file ~config ~from_file inc =
  let candidate_dirs =
    candidate_include_dirs ~config ~from_file
    |> List.map normalize_path
    |> dedup_preserve_order
  in
  let direct_candidate =
    if Filename.is_relative inc then
      None
    else if is_file inc then
      Some (normalize_path inc)
    else
      None
  in
  match direct_candidate with
  | Some f -> f
  | None ->
      let rec try_dirs = function
        | [] ->
            let searched =
              String.concat ", "
                (List.map (fun d -> Filename.concat d inc) candidate_dirs)
            in
            raise
              (Error
                 (Printf.sprintf
                    "Impossible de résoudre include('%s') depuis %s. Candidats essayés: %s"
                    inc from_file searched))
        | dir :: tl ->
            let candidate = Filename.concat dir inc in
            if is_file candidate then
              normalize_path candidate
            else
              try_dirs tl
      in
      try_dirs candidate_dirs

let rec expand_file config visited filename =
  let filename = normalize_path filename in
  if StringSet.mem filename visited then
    raise (Error ("Boucle d'inclusion détectée: " ^ filename));
  let visited = StringSet.add filename visited in
  let entries = parse_file filename |> List.map (set_source_file filename) in
  let rec expand_entries acc = function
    | [] -> List.rev acc
    | Input_include { include_file; include_only } :: tl ->
        let included_file =
          resolve_include_file ~config ~from_file:filename include_file
        in
        let included_entries =
          expand_file config visited included_file
          |> filter_by_name include_only
        in
        expand_entries (List.rev_append included_entries acc) tl
    | x :: tl ->
        expand_entries (x :: acc) tl
  in
  expand_entries [] entries

let load_problem ?(config = default_load_config) filename =
  let inputs = expand_file config StringSet.empty filename in
  let conjecture_count =
    List.fold_left
      (fun count -> function
        | Input_cnf { role = "conjecture"; _ }
        | Input_fof { role = "conjecture"; _ } -> count + 1
        | _ -> count)
      0 inputs
  in
  if conjecture_count > 1 then
    raise
      (Error
         (Printf.sprintf
            "VIP ne supporte qu'une conjecture par problème (trouvé: %d)"
            conjecture_count));
  {
    file = Some filename;
    inputs;
  }
