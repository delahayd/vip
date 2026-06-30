open Tptp_frontend
open Clausify
open Resolution

type szs_status =
  | Theorem
  | Unsatisfiable
  | Satisfiable
  | CounterSatisfiable
  | GaveUp
  | Timeout
  | ResourceOut
  | InputError

type portfolio_mode =
  | Legacy_then_modern
  | Modern_then_legacy
  | Legacy_only
  | Modern_only
  | Modern_compat_only
  | Feq_modern
  | Scheduled_portfolio
  | Experimental_casc
  | Casc_aggressive
  | Casc_240
  | Casc_150
  | Casc_feq_probe

type engine_kind =
  | Legacy_compat
  | Modern_compat_flash
  | Modern_deep
  | Modern_feq

type portfolio_stage = {
  stage_name : string;
  engine : engine_kind;
  time_limit_s : float;
}

type config = {
  time_limit_s : float option;
  max_generated_clauses : int option;
  print_derivation : bool;
  inference_mode : Resolution.inference_mode;
  tptp_dir : string option;
  use_sos : bool;
  portfolio_mode : portfolio_mode;
}

type problem_info = {
  file : string option;
  clause_count : int;
  generated_clause_count : int;
  profile : string;
  raw_clause_count : int;
  equality_literals : int;
  equality_literal_ratio : float;
  avg_literal_term_size : float;
  unit_ratio : float;
  negative_ratio : float;
  axiom_selection_enabled : bool;
}

type outcome = {
  status : szs_status;
  info : problem_info;
  derivation : Resolution.derived list;
  empty_clause : Resolution.derived option;
  resolution_stats : Resolution.stats option;
  tstp_prelude : string option;
}

exception Sat_probe_success of outcome

let default_config = {
  time_limit_s = None;
  max_generated_clauses = None;
  print_derivation = false;
  inference_mode = Resolution.Ordered_with_fallback;
  tptp_dir = None;
  use_sos = true;
  portfolio_mode = Legacy_then_modern;
}

let string_of_szs_status = function
  | Theorem -> "Theorem"
  | Unsatisfiable -> "Unsatisfiable"
  | Satisfiable -> "Satisfiable"
  | CounterSatisfiable -> "CounterSatisfiable"
  | GaveUp -> "GaveUp"
  | Timeout -> "Timeout"
  | ResourceOut -> "ResourceOut"
  | InputError -> "InputError"

let tptp_needs_quotes s =
  let is_lower = function 'a' .. 'z' -> true | _ -> false in
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  String.length s = 0
  || not (is_lower s.[0])
  || not (String.for_all is_ident_char s)

let tptp_quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '\'';
  String.iter
    (function
      | '\'' -> Buffer.add_string b "\\'"
      | '\\' -> Buffer.add_string b "\\\\"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '\'';
  Buffer.contents b

let tptp_name s =
  if tptp_needs_quotes s then tptp_quote s else s

let tptp_var_name v =
  if String.length v > 0 then
    match v.[0] with
    | 'A' .. 'Z' | '_' -> v
    | _ -> String.capitalize_ascii v
  else
    "V"

let rec tptp_term = function
  | Types.Var v -> tptp_var_name v
  | Types.Fun (f, []) -> tptp_name f
  | Types.Fun (f, args) ->
      Printf.sprintf
        "%s(%s)"
        (tptp_name f)
        (String.concat "," (List.map tptp_term args))

let tptp_atom a =
  match a.Types.pred, a.Types.args with
  | "=", [ lhs; rhs ] ->
      Printf.sprintf "%s = %s" (tptp_term lhs) (tptp_term rhs)
  | _, [] -> tptp_name a.Types.pred
  | _ ->
      Printf.sprintf
        "%s(%s)"
        (tptp_name a.Types.pred)
        (String.concat "," (List.map tptp_term a.Types.args))

let rec tptp_formula = function
  | Fof.FTrue -> "$true"
  | Fof.FFalse -> "$false"
  | Fof.Atom a -> tptp_atom a
  | Fof.Not f -> Printf.sprintf "~(%s)" (tptp_formula f)
  | Fof.And (a, b) -> Printf.sprintf "(%s & %s)" (tptp_formula a) (tptp_formula b)
  | Fof.Or (a, b) -> Printf.sprintf "(%s | %s)" (tptp_formula a) (tptp_formula b)
  | Fof.Imp (a, b) -> Printf.sprintf "(%s => %s)" (tptp_formula a) (tptp_formula b)
  | Fof.RevImp (a, b) -> Printf.sprintf "(%s <= %s)" (tptp_formula a) (tptp_formula b)
  | Fof.Iff (a, b) -> Printf.sprintf "(%s <=> %s)" (tptp_formula a) (tptp_formula b)
  | Fof.Xor (a, b) -> Printf.sprintf "(%s <~> %s)" (tptp_formula a) (tptp_formula b)
  | Fof.Forall (vs, f) ->
      Printf.sprintf
        "! [%s] : (%s)"
        (String.concat "," (List.map tptp_var_name vs))
        (tptp_formula f)
  | Fof.Exists (vs, f) ->
      Printf.sprintf
        "? [%s] : (%s)"
        (String.concat "," (List.map tptp_var_name vs))
        (tptp_formula f)

let role_requires_negation role =
  role = "conjecture"

let proof_input_name index =
  Printf.sprintf "vip_input_%d" index

let proof_source_name ~fallback = function
  | "" -> proof_input_name fallback
  | name -> tptp_name name

let proof_source_reference ~fallback = function
  | "" -> "unknown"
  | name -> tptp_name name

let gdv_can_match_source_name name =
  not (tptp_needs_quotes name)

let proof_clause_name id =
  if id < 0 then
    Printf.sprintf "vip_m%d" (-id)
  else
    Printf.sprintf "vip_%d" id

let clause_key clause =
  clause
  |> Clause.normalize_clause
  |> Pretty.string_of_clause

let rec term_has_skolem = function
  | Types.Var _ -> false
  | Types.Fun (f, args) ->
      String.length f >= 2
      && String.sub f 0 2 = "sk"
      || List.exists term_has_skolem args

let atom_has_skolem (a : Types.atom) =
  List.exists term_has_skolem a.args

let literal_has_skolem = function
  | Types.Pos a | Types.Neg a -> atom_has_skolem a

let clause_has_skolem clause =
  List.exists literal_has_skolem clause

let proof_slice_to_root (root : Resolution.derived) derivation =
  let by_id = Hashtbl.create (List.length derivation + 1) in
  List.iter (fun (d : Resolution.derived) -> Hashtbl.replace by_id d.id d) derivation;
  Hashtbl.replace by_id root.id root;
  let needed = Hashtbl.create 257 in
  let rec mark id =
    if not (Hashtbl.mem needed id) then begin
      Hashtbl.add needed id ();
      match Hashtbl.find_opt by_id id with
      | None -> ()
      | Some d -> List.iter mark d.parents
    end
  in
  mark root.id;
  let sliced =
    List.filter
      (fun (d : Resolution.derived) -> Hashtbl.mem needed d.id)
      derivation
  in
  if List.exists (fun (d : Resolution.derived) -> d.id = root.id) sliced then
    sliced
  else
    sliced @ [ root ]

let build_tstp_prelude ?problem inputs (trace : Clausify.clausification_trace) derivation =
  let input_by_index = Hashtbl.create 97 in
  List.iteri
    (fun i input -> Hashtbl.replace input_by_index (i + 1) input)
    inputs;
  let origins_by_clause = Hashtbl.create 257 in
  List.iter
    (fun origin ->
      let key = clause_key origin.Clausify.clause in
      if not (Hashtbl.mem origins_by_clause key) then
        Hashtbl.add origins_by_clause key origin)
    trace.origins;
  let origin_count_by_input = Hashtbl.create 97 in
  List.iter
    (fun origin ->
      let n =
        match Hashtbl.find_opt origin_count_by_input origin.Clausify.input_index with
        | Some n -> n
        | None -> 0
      in
      Hashtbl.replace origin_count_by_input origin.input_index (n + 1))
    trace.origins;
  let find_condensed_origin clause =
    let clause = Clause.normalize_clause clause in
    List.find_opt
      (fun origin ->
        let source = Clause.normalize_clause origin.Clausify.clause in
        List.length clause < List.length source && Clause.subsumes clause source)
      trace.origins
  in
  let printed_inputs = Hashtbl.create 97 in
  let b = Buffer.create 4096 in
  let print_source origin =
    if not (Hashtbl.mem printed_inputs origin.Clausify.input_index) then begin
      Hashtbl.add printed_inputs origin.input_index ();
      let source_name =
        proof_source_name ~fallback:origin.input_index origin.input_name
      in
      let source_reference =
        proof_source_reference ~fallback:origin.input_index origin.input_name
      in
      match Hashtbl.find_opt input_by_index origin.input_index with
      | Some (Fof.Input_fof { role; formula; source_file; _ }) ->
          let source_suffix =
            match source_file, problem with
            | Some p, _ | None, Some p ->
                Printf.sprintf
                  ",file(%s,%s)"
                  (tptp_name p)
                  source_reference
            | None, None -> ""
          in
          Printf.bprintf
            b
            "fof(%s,%s,(%s)%s).\n"
            source_name
            role
            (tptp_formula formula)
            source_suffix
      | Some (Fof.Input_cnf { role; clause; source_file; _ }) ->
          let source_suffix =
            match source_file, problem with
            | Some p, _ | None, Some p ->
                Printf.sprintf
                  ",file(%s,%s)"
                  (tptp_name p)
                  source_reference
            | None, None -> ""
          in
          Printf.bprintf
            b
            "cnf(%s,%s,(%s)%s).\n"
            source_name
            role
            (Resolution.tstp_clause_formula clause)
            source_suffix
      | Some (Fof.Input_include _) | None -> ()
    end
  in
  List.iter
    (fun (d : Resolution.derived) ->
      if d.parents = [] then begin
        let clause = Resolution.tstp_clause_formula d.clause_d in
        match Hashtbl.find_opt origins_by_clause (clause_key d.clause_d) with
        | Some origin when origin.input_is_cnf && not (role_requires_negation origin.input_role) ->
            let source_name =
              proof_source_name ~fallback:origin.input_index origin.input_name
            in
            print_source origin;
            Printf.bprintf
              b
              "cnf(%s,plain,(%s),inference(input_clause_copy,[status(thm)],[%s])).\n"
              (proof_clause_name d.id)
              clause
              source_name
        | Some origin ->
            let source_is_single_clause =
              match Hashtbl.find_opt origin_count_by_input origin.input_index with
              | Some 1 -> gdv_can_match_source_name origin.input_name
              | Some _ | None -> false
            in
            if role_requires_negation origin.input_role
               || source_is_single_clause
               || clause_has_skolem d.clause_d then begin
              print_source origin;
              let source_name =
                proof_source_name ~fallback:origin.input_index origin.input_name
              in
              let role =
                if role_requires_negation origin.input_role then
                  "negated_conjecture"
                else
                  "plain"
              in
              let status =
                if role_requires_negation origin.input_role then "cth" else "esa"
              in
              let status =
                if origin.Clausify.transformation_status <> "" then
                  origin.transformation_status
                else
                  status
              in
              Printf.bprintf
                b
                "cnf(%s,%s,(%s),inference(cnf_transformation,[status(%s)],[%s])).\n"
                (proof_clause_name d.id)
                role
                clause
                status
                source_name
            end else begin
              let role =
                if role_requires_negation origin.input_role then
                  "negated_conjecture"
                else
                  "plain"
              in
              Printf.bprintf
                b
                "cnf(%s,%s,(%s)).\n"
                (proof_clause_name d.id)
                role
                clause
            end
        | None ->
            begin
              match find_condensed_origin d.clause_d with
              | Some origin ->
                  print_source origin;
                  let source_name =
                    proof_source_name ~fallback:origin.input_index origin.input_name
                  in
                  let raw_clause = Resolution.tstp_clause_formula origin.clause in
                  let raw_clause_name = proof_clause_name d.id ^ "_raw" in
                  let role =
                    if role_requires_negation origin.input_role then
                      "negated_conjecture"
                    else
                      "plain"
                  in
                  let status =
                    if origin.Clausify.transformation_status <> "" then
                      origin.transformation_status
                    else if role_requires_negation origin.input_role then
                      "cth"
                    else
                      "esa"
                  in
                  Printf.bprintf
                    b
                    "cnf(%s,%s,(%s),inference(cnf_transformation,[status(%s)],[%s])).\n"
                    raw_clause_name
                    role
                    raw_clause
                    status
                    source_name;
                  Printf.bprintf
                    b
                    "cnf(%s,plain,(%s),inference(condensation,[status(thm)],[%s])).\n"
                    (proof_clause_name d.id)
                    clause
                    raw_clause_name
              | None ->
                  Printf.bprintf b "cnf(%s,axiom,(%s)).\n" (proof_clause_name d.id) clause
            end
      end)
    derivation;
  Buffer.contents b

let clauses_of_file filename =
  let parsed = load_problem filename in
  let clauses, _report = clauses_of_input_with_report parsed.inputs in
  clauses

let input_is_conjecture = function
  | Fof.Input_fof { role; _ } | Fof.Input_cnf { role; _ } ->
      role = "conjecture"
  | Fof.Input_include _ -> false

let infer_status_from_stop_reason ~has_conjecture = function
  | Refutation_found _ ->
      if has_conjecture then Theorem else Unsatisfiable
  | Saturation -> GaveUp
  | Time_limit -> Timeout
  | Clause_limit -> ResourceOut

let resolution_mode_of_legacy = function
  | Resolution.Unrestricted -> Legacy_resolution.Unrestricted
  | Resolution.Ordered -> Legacy_resolution.Ordered
  | Resolution.Ordered_with_fallback -> Legacy_resolution.Ordered_with_fallback

let derived_of_legacy (d : Legacy_resolution.derived) : Resolution.derived =
  {
    Resolution.id = d.id;
    parents = d.parents;
    rule = d.rule;
    clause_d = d.clause_d;
    is_active = false;
  }

let result_of_legacy (l_res : Legacy_resolution.run_result) : Resolution.run_result =
  {
    Resolution.stop_reason =
      (match l_res.stop_reason with
       | Legacy_resolution.Refutation_found d ->
           Resolution.Refutation_found (derived_of_legacy d)
       | Legacy_resolution.Saturation -> Resolution.Saturation
       | Legacy_resolution.Time_limit -> Resolution.Time_limit
       | Legacy_resolution.Clause_limit -> Resolution.Clause_limit);
    derivation = List.map derived_of_legacy l_res.derivation;
    stats =
      {
        Resolution.generated_clauses = l_res.stats.generated_clauses;
        processed_clauses = l_res.stats.processed_clauses;
        resolution_inferences = l_res.stats.resolution_inferences;
        factoring_inferences = l_res.stats.factoring_inferences;
        equality_resolution_inferences = l_res.stats.equality_resolution_inferences;
        equality_factoring_inferences = l_res.stats.equality_factoring_inferences;
        superposition_inferences = l_res.stats.superposition_inferences;
        demodulation_rewrites = l_res.stats.demodulation_rewrites;
        subsumption_tests = l_res.stats.subsumption_tests;
        subsumption_rejections = l_res.stats.subsumption_rejections;
        avatar_enabled = false;
        avatar_keep_original = false;
        avatar_min_split_literals = 0;
        avatar_max_split_vars = 0;
        avatar_split_vars_used = 0;
        avatar_split_attempts = 0;
        avatar_successful_splits = 0;
        avatar_split_components = 0;
        avatar_split_rejected_disabled = 0;
        avatar_split_rejected_short = 0;
        avatar_split_rejected_equality = 0;
        avatar_split_rejected_nonground = 0;
        avatar_split_rejected_trivial = 0;
        avatar_split_rejected_quota = 0;
        avatar_contextual_empty_conflicts = 0;
        avatar_sat_clauses_added = 0;
        avatar_sat_solves = 0;
        avatar_sat_conflicts = 0;
        avatar_context_sat_tests = 0;
        avatar_context_sat_failures = 0;
        avatar_filtered_inferences = 0;
        wall_clock_s = l_res.stats.wall_clock_s;
      };
  }


let legacy_env_name name =
  let prefix = "VIP_" in
  let lp = String.length prefix in
  if String.length name >= lp && String.sub name 0 lp = prefix then
    Some ("IP_" ^ String.sub name lp (String.length name - lp))
  else
    None

let env_opt name =
  match Sys.getenv_opt name with
  | Some _ as v -> v
  | None ->
      match legacy_env_name name with
      | Some legacy -> Sys.getenv_opt legacy
      | None -> None

let getenv_bool name default =
  match env_opt name with
  | None -> default
  | Some s ->
      begin
        match String.lowercase_ascii (String.trim s) with
        | "1" | "true" | "yes" | "on" -> true
        | "0" | "false" | "no" | "off" -> false
        | _ -> default
      end

let getenv_int_global name default =
  match env_opt name with
  | None -> default
  | Some s ->
      (try max 0 (int_of_string s) with Failure _ -> default)

let rec ground_term_key = function
  | Types.Var _ -> None
  | Types.Fun (f, []) -> Some f
  | Types.Fun (f, args) ->
      let rec aux acc = function
        | [] -> Some (f ^ "(" ^ String.concat "," (List.rev acc) ^ ")")
        | t :: tl ->
            match ground_term_key t with
            | None -> None
            | Some k -> aux (k :: acc) tl
      in
      aux [] args

let ground_atom_key (a : Types.atom) =
  match a.args with
  | [] -> Some a.pred
  | args ->
      let rec aux acc = function
        | [] -> Some (a.pred ^ "(" ^ String.concat "," (List.rev acc) ^ ")")
        | t :: tl ->
            match ground_term_key t with
            | None -> None
            | Some k -> aux (k :: acc) tl
      in
      aux [] args

let ground_literal_key = function
  | Types.Pos { pred = "="; _ } | Types.Neg { pred = "="; _ } -> None
  | Types.Pos a -> Option.map (fun k -> k, true) (ground_atom_key a)
  | Types.Neg a -> Option.map (fun k -> k, false) (ground_atom_key a)

let empty_resolution_stats wall_clock_s =
  {
    Resolution.generated_clauses = 0;
    processed_clauses = 0;
    resolution_inferences = 0;
    factoring_inferences = 0;
    equality_resolution_inferences = 0;
    equality_factoring_inferences = 0;
    superposition_inferences = 0;
    demodulation_rewrites = 0;
    subsumption_tests = 0;
    subsumption_rejections = 0;
    avatar_enabled = false;
    avatar_keep_original = false;
    avatar_min_split_literals = 0;
    avatar_max_split_vars = 0;
    avatar_split_vars_used = 0;
    avatar_split_attempts = 0;
    avatar_successful_splits = 0;
    avatar_split_components = 0;
    avatar_split_rejected_disabled = 0;
    avatar_split_rejected_short = 0;
    avatar_split_rejected_equality = 0;
    avatar_split_rejected_nonground = 0;
    avatar_split_rejected_trivial = 0;
    avatar_split_rejected_quota = 0;
    avatar_contextual_empty_conflicts = 0;
    avatar_sat_clauses_added = 0;
    avatar_sat_solves = 0;
    avatar_sat_conflicts = 0;
    avatar_context_sat_tests = 0;
    avatar_context_sat_failures = 0;
    avatar_filtered_inferences = 0;
    wall_clock_s;
  }

let ground_sat_unsat clauses =
  let ids = Hashtbl.create 257 in
  let prop_ids = Hashtbl.create 64 in
  let next_id = ref 0 in
  let id_of_key key =
    match Hashtbl.find_opt ids key with
    | Some id -> id
    | None ->
        incr next_id;
        Hashtbl.add ids key !next_id;
        if not (String.contains key '(') then Hashtbl.add prop_ids !next_id true;
        !next_id
  in
  let encode_lit lit =
    match ground_literal_key lit with
    | None -> None
    | Some (key, positive) ->
        let id = id_of_key key in
        Some (if positive then id else -id)
  in
  let encode_clause c =
    let rec aux acc = function
      | [] -> Some (List.sort_uniq compare acc)
      | lit :: tl ->
          match encode_lit lit with
          | None -> None
          | Some i -> aux (i :: acc) tl
    in
    match aux [] c with
    | None -> None
    | Some lits ->
        let sorted = List.sort_uniq compare lits in
        if List.exists (fun lit -> List.exists (( = ) (-lit)) sorted) sorted then
          Some []
        else
          Some sorted
  in
  let rec encode acc = function
    | [] -> Some (List.rev acc)
    | c :: tl ->
        match encode_clause c with
        | None -> None
        | Some [] -> encode acc tl
        | Some c' -> encode (c' :: acc) tl
  in
  let dpll_array var_count prop_var clauses =
    let assignment = Array.make (var_count + 1) 0 in
    let trail = ref [] in
    let assign lit =
      let v = abs lit in
      let value = if lit > 0 then 1 else -1 in
      match assignment.(v) with
      | 0 ->
          assignment.(v) <- value;
          trail := v :: !trail;
          true
      | old -> old = value
    in
    let undo mark =
      let rec aux n =
        if n <= mark then ()
        else
          match !trail with
          | [] -> ()
          | v :: tl ->
              assignment.(v) <- 0;
              trail := tl;
              aux (n - 1)
      in
      aux (List.length !trail)
    in
    let lit_value lit =
      let v = abs lit in
      match assignment.(v) with
      | 0 -> 0
      | value -> if (lit > 0 && value = 1) || (lit < 0 && value = -1) then 1 else -1
    in
    let assign_pure_literals () =
      let masks = Array.make (var_count + 1) 0 in
      Array.iter
        (fun clause ->
          let clause_satisfied = Array.exists (fun lit -> lit_value lit = 1) clause in
          if not clause_satisfied then
            Array.iter
              (fun lit ->
                let v = abs lit in
                if assignment.(v) = 0 then
                  let mask = if lit > 0 then 1 else 2 in
                  masks.(v) <- masks.(v) lor mask)
              clause)
        clauses;
      let changed = ref false in
      let ok = ref true in
      for v = 1 to var_count do
        if !ok && assignment.(v) = 0 then
          match masks.(v) with
          | 1 -> if assign v then changed := true else ok := false
          | 2 -> if assign (-v) then changed := true else ok := false
          | _ -> ()
      done;
      (!ok, !changed)
    in
    let propagate () =
      let changed = ref true in
      let ok = ref true in
      while !ok && !changed do
        changed := false;
        Array.iter
          (fun clause ->
            if !ok then begin
              let satisfied = ref false in
              let unassigned = ref 0 in
              let last_unassigned = ref 0 in
              Array.iter
                (fun lit ->
                  match lit_value lit with
                  | 1 -> satisfied := true
                  | 0 -> incr unassigned; last_unassigned := lit
                  | _ -> ())
                clause;
              if not !satisfied then
                if !unassigned = 0 then ok := false
                else if !unassigned = 1 then
                  if assign !last_unassigned then changed := true else ok := false
            end)
          clauses;
        if !ok && not !changed then
          let pure_ok, pure_changed = assign_pure_literals () in
          ok := pure_ok;
          changed := pure_changed
      done;
      !ok
    in
    let all_satisfied () =
      Array.for_all
        (fun clause -> Array.exists (fun lit -> lit_value lit = 1) clause)
        clauses
    in
    let choose_lit_array () =
      let counts = Array.make (var_count + 1) (0, 0) in
      Array.iter
        (fun clause ->
          let clause_satisfied = Array.exists (fun lit -> lit_value lit = 1) clause in
          if not clause_satisfied then
            Array.iter
              (fun lit ->
                let v = abs lit in
                if assignment.(v) = 0 then
                  let pos, neg = counts.(v) in
                  if lit > 0 then counts.(v) <- (pos + 1, neg)
                  else counts.(v) <- (pos, neg + 1))
              clause)
        clauses;
      let best = ref None in
      for v = 1 to var_count do
        if assignment.(v) = 0 then
          let pos, neg = counts.(v) in
          let score = pos + neg + if prop_var.(v) then 1_000_000 else 0 in
          if score > 0 then
            match !best with
            | None -> best := Some (v, pos, neg, score)
            | Some (_, _, _, best_score) when score > best_score ->
                best := Some (v, pos, neg, score)
            | _ -> ()
      done;
      match !best with
      | None -> None
      | Some (v, pos, neg, _) -> Some (if pos >= neg then v else -v)
    in
    let rec search () =
      if not (propagate ()) then false
      else if all_satisfied () then true
      else
        match choose_lit_array () with
        | None -> true
        | Some lit ->
            let mark = List.length !trail in
            let left = assign lit && search () in
            if left then true
            else begin
              undo mark;
              let right = assign (-lit) && search () in
              if right then true else (undo mark; false)
            end
    in
    search ()
  in
  let debug = getenv_bool "VIP_GROUND_SAT_DEBUG" false in
  if debug then Printf.eprintf "[ground-sat] encoding clauses\n%!";
  match encode [] clauses with
  | None -> None
  | Some clauses ->
      if debug then
        Printf.eprintf "[ground-sat] encoded clauses=%d vars=%d, solving\n%!"
          (List.length clauses)
          !next_id;
      let prop_var = Array.make (!next_id + 1) false in
      Hashtbl.iter (fun id _ -> if id <= !next_id then prop_var.(id) <- true) prop_ids;
      let clauses = Array.of_list (List.map Array.of_list clauses) in
      Some (not (dpll_array !next_id prop_var clauses))

let rec term_is_epr = function
  | Types.Var _ -> true
  | Types.Fun (_, []) -> true
  | Types.Fun (_, _) -> false

let atom_is_epr (a : Types.atom) = List.for_all term_is_epr a.args

let literal_is_epr = function
  | Types.Pos { pred = "="; _ } | Types.Neg { pred = "="; _ } -> false
  | Types.Pos a | Types.Neg a -> atom_is_epr a

let rec vars_of_term acc = function
  | Types.Var v -> Types.StringSet.add v acc
  | Types.Fun (_, args) -> List.fold_left vars_of_term acc args

let vars_of_atom acc (a : Types.atom) =
  List.fold_left vars_of_term acc a.args

let vars_of_literal acc = function
  | Types.Pos a | Types.Neg a -> vars_of_atom acc a

let vars_of_clause c =
  c
  |> List.fold_left vars_of_literal Types.StringSet.empty
  |> Types.StringSet.elements

let rec constants_of_term acc = function
  | Types.Var _ -> acc
  | Types.Fun (f, []) -> Types.StringSet.add f acc
  | Types.Fun (_, args) -> List.fold_left constants_of_term acc args

let constants_of_atom acc (a : Types.atom) =
  List.fold_left constants_of_term acc a.args

let constants_of_literal acc = function
  | Types.Pos a | Types.Neg a -> constants_of_atom acc a

let constants_of_clauses clauses =
  clauses
  |> List.fold_left (fun acc c -> List.fold_left constants_of_literal acc c) Types.StringSet.empty
  |> Types.StringSet.elements

let pow_capped base exp cap =
  let rec aux acc n =
    if n = 0 then Some acc
    else if base <> 0 && acc > cap / base then None
    else aux (acc * base) (n - 1)
  in
  aux 1 exp

let rec substitute_term subst = function
  | Types.Var v ->
      begin
        match Types.StringMap.find_opt v subst with
        | Some t -> t
        | None -> Types.Var v
      end
  | Types.Fun (f, args) -> Types.Fun (f, List.map (substitute_term subst) args)

let substitute_atom subst (a : Types.atom) =
  { a with Types.args = List.map (substitute_term subst) a.args }

let substitute_literal subst = function
  | Types.Pos a -> Types.Pos (substitute_atom subst a)
  | Types.Neg a -> Types.Neg (substitute_atom subst a)

let substitute_clause subst c = List.map (substitute_literal subst) c

let epr_ground_instances ~max_instances clauses =
  if not (List.for_all (List.for_all literal_is_epr) clauses) then None
  else
    let constants = constants_of_clauses clauses in
    let constants = if constants = [] then [ "epr_default" ] else constants in
    let constants = List.map (fun c -> Types.Fun (c, [])) constants in
    let base = List.length constants in
    let total = ref 0 in
    let clause_vars = List.map vars_of_clause clauses in
    let add_count vars =
      match pow_capped base (List.length vars) (max_instances - !total) with
      | None -> false
      | Some n -> total := !total + n; !total <= max_instances
    in
    if not (List.for_all add_count clause_vars) then None
    else
      let rec assignments vars =
        match vars with
        | [] -> [ Types.StringMap.empty ]
        | v :: tl ->
            let tails = assignments tl in
            List.concat
              (List.map
                 (fun const ->
                   List.map
                     (fun subst -> Types.StringMap.add v const subst)
                     tails)
                 constants)
      in
      let grounded =
        List.concat
          (List.map2
             (fun c vars ->
               List.map (fun subst -> substitute_clause subst c) (assignments vars))
             clauses
             clause_vars)
      in
      Some grounded

let epr_ground_sat_unsat ~max_instances clauses =
  let debug = getenv_bool "VIP_GROUND_SAT_DEBUG" false in
  if debug then
    Printf.eprintf "[ground-sat] attempting epr grounding, max_instances=%d\n%!" max_instances;
  match epr_ground_instances ~max_instances clauses with
  | None ->
      if debug then Printf.eprintf "[ground-sat] epr grounding skipped\n%!";
      None
  | Some grounded ->
      if debug then
        Printf.eprintf "[ground-sat] grounded clauses=%d, entering dpll\n%!" (List.length grounded);
      let res = ground_sat_unsat grounded in
      if debug then
        Printf.eprintf "[ground-sat] dpll result=%s\n%!"
          (match res with None -> "ineligible" | Some true -> "unsat" | Some false -> "sat");
      res

let rec add_term_symbols acc = function
  | Types.Var _ -> acc
  | Types.Fun (f, args) ->
      List.fold_left add_term_symbols (Types.StringSet.add ("f:" ^ f) acc) args

let add_atom_symbols acc (a : Types.atom) =
  List.fold_left add_term_symbols (Types.StringSet.add ("p:" ^ a.pred) acc) a.args

let add_literal_symbols acc = function
  | Types.Pos a | Types.Neg a -> add_atom_symbols acc a

let clause_symbols c =
  List.fold_left add_literal_symbols Types.StringSet.empty c

let clauses_symbols clauses =
  List.fold_left
    (fun acc c -> Types.StringSet.union acc (clause_symbols c))
    Types.StringSet.empty
    clauses

type problem_profile =
  | Equality_heavy
  | Equality_light
  | Non_equality
  | Large_general

let string_of_problem_profile = function
  | Equality_heavy -> "equality-heavy"
  | Equality_light -> "equality-light"
  | Non_equality -> "non-equality"
  | Large_general -> "large-general"

let select_axioms_sine ~enabled ~check_timeout ~support axioms =
  if not enabled then
    axioms
  else
    let min_axioms = getenv_int_global "VIP_AXIOM_SELECTION_MIN_AXIOMS" 100 in
    if List.length axioms < min_axioms then
      axioms
    else
      let default_rounds = if support = [] then 1 else 6 in
      let max_rounds = getenv_int_global "VIP_AXIOM_SELECTION_ROUNDS" default_rounds in
      let max_axioms =
        match env_opt "VIP_AXIOM_SELECTION_MAX_AXIOMS" with
        | None -> 300
        | Some s -> (try max 0 (int_of_string s) with Failure _ -> 300)
      in
      let axiom_infos =
        axioms
        |> List.mapi (fun i c -> (i, c, clause_symbols c))
      in
      let symbol_counts = Hashtbl.create 1024 in
      List.iter
        (fun (_i, _c, syms) ->
          check_timeout ();
          Types.StringSet.iter
            (fun s ->
              let n = Option.value (Hashtbl.find_opt symbol_counts s) ~default:0 in
              Hashtbl.replace symbol_counts s (n + 1))
            syms)
        axiom_infos;
      let symbol_frequency s =
        Option.value (Hashtbl.find_opt symbol_counts s) ~default:0
      in
      let max_symbol_freq =
        getenv_int_global "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ" 128
      in
      let selectable_symbols syms =
        Types.StringSet.filter
          (fun s ->
            let freq = symbol_frequency s in
            (freq = 0 || freq <= max_symbol_freq) && not (String.equal s "p:="))
          syms
      in
      let rare_seed_symbols () =
        let max_freq = getenv_int_global "VIP_AXIOM_SELECTION_RARE_MAX_FREQ" 2 in
        let max_seeds = getenv_int_global "VIP_AXIOM_SELECTION_RARE_MAX_SYMBOLS" 64 in
        let candidates =
          Hashtbl.fold
            (fun s n acc ->
              if n <= max_freq && not (String.equal s "p:=") then (n, s) :: acc
              else acc)
            symbol_counts
            []
          |> List.sort compare
        in
        let rec take n acc = function
          | [] -> acc
          | _ when n <= 0 -> acc
          | (_freq, s) :: tl -> take (n - 1) (Types.StringSet.add s acc) tl
        in
        take max_seeds Types.StringSet.empty candidates
      in
      let predicate_symbols_only syms =
        Types.StringSet.filter
          (fun s -> String.length s >= 2 && String.sub s 0 2 = "p:")
          syms
      in
      let initial_symbols =
        if support = [] then rare_seed_symbols ()
        else
          let syms = selectable_symbols (clauses_symbols support) in
          if getenv_bool "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY" false then
            let pred_syms = predicate_symbols_only syms in
            if Types.StringSet.is_empty pred_syms then syms else pred_syms
          else
            syms
      in
      let selected = Hashtbl.create (List.length axioms) in
      let selected_symbols = ref initial_symbols in
      let selected_count = ref 0 in
      let add_axiom i syms =
        if not (Hashtbl.mem selected i) && !selected_count < max_axioms then begin
          Hashtbl.add selected i ();
          incr selected_count;
          selected_symbols :=
            Types.StringSet.union !selected_symbols (selectable_symbols syms);
          true
        end else
          false
      in
      let relevant syms =
        not (Types.StringSet.is_empty (Types.StringSet.inter syms !selected_symbols))
      in
      let ranked_selection = getenv_bool "VIP_AXIOM_SELECTION_RANKED" false in
      let overlap_count syms =
        Types.StringSet.cardinal (Types.StringSet.inter syms !selected_symbols)
      in
      let min_selected_frequency syms =
        let overlap = Types.StringSet.inter syms !selected_symbols in
        if Types.StringSet.is_empty overlap then
          max_int
        else
          Types.StringSet.fold
            (fun s acc -> min acc (symbol_frequency s))
            overlap
            max_int
      in
      let compare_candidate (i1, c1, syms1) (i2, c2, syms2) =
        let r1 = min_selected_frequency syms1 in
        let r2 = min_selected_frequency syms2 in
        let c = compare r1 r2 in
        if c <> 0 then c
        else
          let o1 = overlap_count syms1 in
          let o2 = overlap_count syms2 in
          let c = compare o2 o1 in
          if c <> 0 then c
          else
            let c = compare (Types.StringSet.cardinal syms1) (Types.StringSet.cardinal syms2) in
            if c <> 0 then c
            else
              let c = compare (List.length c1) (List.length c2) in
              if c <> 0 then c else compare i1 i2
      in
      let rec rounds n =
        check_timeout ();
        if n <= 0 then ()
        else
          let changed = ref false in
          if ranked_selection then begin
            let candidates =
              axiom_infos
              |> List.filter
                   (fun (i, _c, syms) ->
                     check_timeout ();
                     (not (Hashtbl.mem selected i)) && relevant syms)
              |> List.sort compare_candidate
            in
            List.iter
              (fun (i, _c, syms) ->
                check_timeout ();
                if add_axiom i syms then changed := true)
              candidates
          end else
            List.iter
              (fun (i, _c, syms) ->
                check_timeout ();
                if relevant syms && add_axiom i syms then changed := true)
              axiom_infos;
          if !changed then rounds (n - 1)
      in
      if Types.StringSet.is_empty initial_symbols then
        axioms
      else begin
        rounds max_rounds;
        if getenv_bool "VIP_AXIOM_SELECTION_DEBUG" false then
          Printf.eprintf
            "[axiom-selection] axioms=%d selected=%d support=%d rounds=%d seed_symbols=%d\n%!"
            (List.length axioms)
            !selected_count
            (List.length support)
            max_rounds
            (Types.StringSet.cardinal initial_symbols);
        if !selected_count = 0 then
          axioms
        else
          axiom_infos
          |> List.filter_map
               (fun (i, c, _syms) -> if Hashtbl.mem selected i then Some c else None)
      end

let rec run_file ?(config = default_config) filename =
  reset_fresh_state ();
  Clause.reset_fresh_counter ();
  Resolution.reset_id_counter ();
  Legacy_resolution.reset_id_counter ();
  let started_at = Unix.gettimeofday () in
  let total_timeout =
    match config.time_limit_s with
    | Some t -> t
    | None -> 6.0
  in
  let elapsed () = Unix.gettimeofday () -. started_at in
  let check_problem_timeout () =
    match config.time_limit_s with
    | Some _ when elapsed () >= total_timeout -> raise Clausify.Timeout_hit
    | _ -> ()
  in
  let timeout_outcome ?(clause_count = 0) wall_clock_s =
    {
      status = Timeout;
      info = {
        file = Some filename;
        clause_count;
        generated_clause_count = 0;
        profile = "unknown";
        raw_clause_count = clause_count;
        equality_literals = 0;
        equality_literal_ratio = 0.0;
        avg_literal_term_size = 0.0;
        unit_ratio = 0.0;
        negative_ratio = 0.0;
        axiom_selection_enabled = false;
      };
      derivation = [];
      empty_clause = None;
      resolution_stats =
        Some
          {
            Resolution.generated_clauses = 0;
            processed_clauses = 0;
            resolution_inferences = 0;
            factoring_inferences = 0;
            equality_resolution_inferences = 0;
            equality_factoring_inferences = 0;
            superposition_inferences = 0;
            demodulation_rewrites = 0;
            subsumption_tests = 0;
            subsumption_rejections = 0;
            avatar_enabled = false;
            avatar_keep_original = false;
            avatar_min_split_literals = 0;
            avatar_max_split_vars = 0;
            avatar_split_vars_used = 0;
            avatar_split_attempts = 0;
            avatar_successful_splits = 0;
            avatar_split_components = 0;
            avatar_split_rejected_disabled = 0;
            avatar_split_rejected_short = 0;
            avatar_split_rejected_equality = 0;
            avatar_split_rejected_nonground = 0;
            avatar_split_rejected_trivial = 0;
            avatar_split_rejected_quota = 0;
            avatar_contextual_empty_conflicts = 0;
            avatar_sat_clauses_added = 0;
            avatar_sat_solves = 0;
            avatar_sat_conflicts = 0;
            avatar_context_sat_tests = 0;
            avatar_context_sat_failures = 0;
            avatar_filtered_inferences = 0;
            wall_clock_s;
          };
      tstp_prelude = None;
    }
  in

  try
    let load_config =
      match config.tptp_dir with
      | None -> default_load_config
      | Some d -> { include_paths = [ d ]; use_tptp_env = true }
    in
    let parsed = load_problem ~config:load_config filename in
    let has_conjecture = List.exists input_is_conjecture parsed.inputs in
    check_problem_timeout ();
    let traced_part =
      partition_input_clauses_with_trace
        ~check_timeout:check_problem_timeout
        parsed.inputs
    in
    let part = traced_part.partition in

    let raw_axioms, support =
      if config.use_sos then (part.axioms, part.support)
      else ([], part.axioms @ part.support)
    in
    let literal_contains_equality = function
      | Types.Pos { pred = "="; args = [ _; _ ] }
      | Types.Neg { pred = "="; args = [ _; _ ] } -> true
      | _ -> false
    in
    let literal_is_negative = function
      | Types.Neg _ -> true
      | Types.Pos _ -> false
    in
    let term_size =
      let rec aux = function
        | Types.Var _ -> 1
        | Types.Fun (_, args) ->
            1 + List.fold_left (fun acc t -> acc + aux t) 0 args
      in
      aux
    in
    let literal_term_size = function
      | Types.Pos a | Types.Neg a ->
          List.fold_left (fun acc t -> acc + term_size t) 1 a.args
    in
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
      let ratio n d =
        if d = 0 then 0.0 else float_of_int n /. float_of_int d
      in
      let avg_clause_len = ratio total_literals clause_count in
      let avg_literal_term_size = ratio total_term_size total_literals in
      let equality_literal_ratio = ratio equality_literals total_literals in
      let negative_ratio = ratio negative_literals total_literals in
      let unit_ratio = ratio unit_clauses clause_count in
      ( clause_count,
        total_literals,
        equality_literals,
        equality_literal_ratio,
        avg_clause_len,
        avg_literal_term_size,
        negative_ratio,
        unit_ratio )
    in
    let raw_all_clauses = raw_axioms @ support in
    let ( raw_clause_count,
          _raw_total_literals,
          raw_equality_literals,
          raw_equality_literal_ratio,
          _raw_avg_clause_len,
          raw_avg_literal_term_size,
          _raw_negative_ratio,
          _raw_unit_ratio ) =
      clause_stats raw_all_clauses
    in
    let raw_equality_problem = raw_equality_literals > 0 in
    let equality_heavy =
      raw_equality_literals >= getenv_int_global "VIP_PROFILE_EQ_HEAVY_MIN_LITERALS" 4
      && raw_equality_literal_ratio
         >= (match env_opt "VIP_PROFILE_EQ_HEAVY_MIN_RATIO" with
             | Some s -> (try float_of_string s with Failure _ -> 0.12)
             | None -> 0.12)
      && raw_avg_literal_term_size
         >= (match env_opt "VIP_PROFILE_EQ_HEAVY_MIN_AVG_TERM" with
             | Some s -> (try float_of_string s with Failure _ -> 3.5)
             | None -> 3.5)
    in
    let problem_profile =
      if equality_heavy then Equality_heavy
      else if raw_equality_problem then Equality_light
      else if raw_clause_count >= getenv_int_global "VIP_PROFILE_LARGE_MIN_CLAUSES" 500 then Large_general
      else Non_equality
    in
    let axiom_selection_enabled =
      getenv_bool "VIP_AXIOM_SELECTION" false
      && (problem_profile = Equality_heavy || getenv_bool "VIP_AXIOM_SELECTION_ALL" false)
    in
    let axioms =
      select_axioms_sine
        ~enabled:axiom_selection_enabled
        ~check_timeout:check_problem_timeout
        ~support
        raw_axioms
    in
    let clause_count = List.length axioms + List.length support in
    let remaining_time () = max 0.0 (total_timeout -. elapsed ()) in
    let stage_time requested = min requested (remaining_time ()) in

    let zero_resolution_stats wall_clock_s =
      {
        generated_clauses = 0;
        processed_clauses = 0;
        resolution_inferences = 0;
          factoring_inferences = 0;
          equality_resolution_inferences = 0;
          equality_factoring_inferences = 0;
          superposition_inferences = 0;
          demodulation_rewrites = 0;
          subsumption_tests = 0;
          subsumption_rejections = 0;
          avatar_enabled = false;
          avatar_keep_original = false;
          avatar_min_split_literals = 0;
          avatar_max_split_vars = 0;
          avatar_split_vars_used = 0;
          avatar_split_attempts = 0;
          avatar_successful_splits = 0;
          avatar_split_components = 0;
          avatar_split_rejected_disabled = 0;
          avatar_split_rejected_short = 0;
          avatar_split_rejected_equality = 0;
          avatar_split_rejected_nonground = 0;
          avatar_split_rejected_trivial = 0;
          avatar_split_rejected_quota = 0;
          avatar_contextual_empty_conflicts = 0;
          avatar_sat_clauses_added = 0;
          avatar_sat_solves = 0;
          avatar_sat_conflicts = 0;
          avatar_context_sat_tests = 0;
        avatar_context_sat_failures = 0;
        avatar_filtered_inferences = 0;
        wall_clock_s;
      }
    in
    let timeout_result wall_clock_s =
      {
        Resolution.stop_reason = Time_limit;
        derivation = [];
        stats = zero_resolution_stats wall_clock_s;
      }
    in

    let run_legacy_compat ~time_limit_s =
      let limits = {
        Legacy_resolution.time_limit_s = Some time_limit_s;
        max_generated_clauses = config.max_generated_clauses;
      } in
      try
        Legacy_resolution.run_resolution_sos
          ~limits
          ~mode:(resolution_mode_of_legacy config.inference_mode)
          ~axioms
          ~support
          ()
        |> result_of_legacy
      with Legacy_resolution.Timeout_hit ->
        timeout_result time_limit_s
    in

    let all_clauses = axioms @ support in
    let ( _clause_count,
          total_literals,
          equality_literals,
          equality_literal_ratio,
          avg_clause_len,
          avg_literal_term_size,
          negative_ratio,
          unit_ratio ) =
      clause_stats all_clauses
    in
    let equality_problem = equality_literals > 0 in
    if getenv_bool "VIP_PROFILE_DEBUG" false then
      Printf.eprintf
        "[profile] profile=%s raw_clauses=%d clauses=%d literals=%d eq_literals=%d eq_ratio=%.3f avg_term=%.2f unit_ratio=%.2f negative_ratio=%.2f axiom_selection=%b\n%!"
        (string_of_problem_profile problem_profile)
        raw_clause_count
        clause_count
        total_literals
        equality_literals
        equality_literal_ratio
        avg_literal_term_size
        unit_ratio
        negative_ratio
        axiom_selection_enabled;

    let input_has_conjecture =
      List.exists
        (function
          | Fof.Input_fof { role; _ }
          | Fof.Input_cnf { role; _ } ->
              role = "conjecture" || role = "negated_conjecture"
          | Fof.Input_include _ -> false)
        parsed.inputs
    in

    if getenv_bool "VIP_SAT_PROBE" false then begin
      let max_instances = getenv_int_global "VIP_SAT_PROBE_MAX_INSTANCES" 20_000 in
      match
        Sat_probe.run
          ~max_instances
          ~check_timeout:check_problem_timeout
          raw_all_clauses
      with
      | Sat_probe.Satisfiable ->
          let status =
            if input_has_conjecture then CounterSatisfiable else Satisfiable
          in
          if getenv_bool "VIP_SAT_PROBE_DEBUG" false then
            Printf.eprintf
              "[sat-probe] status=%s clauses=%d max_instances=%d\n%!"
              (string_of_szs_status status)
              raw_clause_count
              max_instances;
          raise (Sat_probe_success {
            status;
            info = {
              file = Some filename;
              clause_count = raw_clause_count;
              generated_clause_count = 0;
              profile = string_of_problem_profile problem_profile;
              raw_clause_count;
              equality_literals = raw_equality_literals;
              equality_literal_ratio = raw_equality_literal_ratio;
              avg_literal_term_size = raw_avg_literal_term_size;
              unit_ratio;
              negative_ratio;
              axiom_selection_enabled = false;
            };
            derivation = [];
            empty_clause = None;
            resolution_stats = Some (zero_resolution_stats (elapsed ()));
            tstp_prelude = None;
          })
      | Sat_probe.Not_applicable reason ->
          if getenv_bool "VIP_SAT_PROBE_DEBUG" false then
            Printf.eprintf "[sat-probe] not-applicable: %s\n%!" reason
      | Sat_probe.Too_large reason ->
          if getenv_bool "VIP_SAT_PROBE_DEBUG" false then
            Printf.eprintf "[sat-probe] too-large: %s\n%!" reason
      | Sat_probe.Unsat ->
          if getenv_bool "VIP_SAT_PROBE_DEBUG" false then
            Printf.eprintf "[sat-probe] finite EPR instances unsat\n%!"
    end;

    let compat_mode () =
      if equality_problem then Resolution.Unrestricted else config.inference_mode
    in

    let run_modern_resolution ?mode_override ~time_limit_s ~expensive_simplifications ~emulate_v1 () =
      let limits = {
        Resolution.time_limit_s = Some time_limit_s;
        max_generated_clauses = config.max_generated_clauses;
      } in
      let mode =
        match mode_override with
        | Some mode -> mode
        | None -> if emulate_v1 then compat_mode () else config.inference_mode
      in
      try
        Resolution.run_resolution_sos
          ~limits
          ~expensive_simplifications
          ~emulate_v1
          ~mode
          ~axioms
          ~support
          ()
      with Resolution.Timeout_hit ->
        timeout_result time_limit_s
    in

    let run_modern_compat_flash ~time_limit_s =
      let use_deep_compat =
        ((not equality_problem) && clause_count >= 100)
        || (equality_problem && clause_count > 5)
      in
      run_modern_resolution
        ~time_limit_s
        ~expensive_simplifications:use_deep_compat
        ~emulate_v1:(not use_deep_compat)
        ()
    in

    let run_modern_deep ~time_limit_s =
      run_modern_resolution
        ~time_limit_s
        ~expensive_simplifications:true
        ~emulate_v1:false
        ()
    in

    let run_modern_feq ~time_limit_s =
      run_modern_resolution
        ~mode_override:Resolution.Unrestricted
        ~time_limit_s
        ~expensive_simplifications:true
        ~emulate_v1:false
        ()
    in

    let run_stage stage =
      if config.print_derivation then
        Printf.printf
          "%% Stage: %s (%.2fs)\n%!"
          stage.stage_name
          stage.time_limit_s;
      check_problem_timeout ();
      if stage.time_limit_s <= 0.0 then timeout_result (elapsed ())
      else
        match stage.engine with
        | Legacy_compat -> run_legacy_compat ~time_limit_s:stage.time_limit_s
        | Modern_compat_flash ->
            run_modern_compat_flash ~time_limit_s:stage.time_limit_s
        | Modern_deep -> run_modern_deep ~time_limit_s:stage.time_limit_s
        | Modern_feq -> run_modern_feq ~time_limit_s:stage.time_limit_s
    in

    let getenv_float name default =
      match env_opt name with
      | None -> default
      | Some s ->
          (try max 0.0 (float_of_string s) with Failure _ -> default)
    in

    let getenv_int name default =
      match env_opt name with
      | None -> default
      | Some s ->
          (try max 0 (int_of_string s) with Failure _ -> default)
    in

    let getenv_float_opt name =
      match env_opt name with
      | None -> None
      | Some s ->
          (try Some (max 0.0 (float_of_string s)) with Failure _ -> None)
    in

    let clamp_fraction x =
      if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x
    in

    let fraction_budget fraction =
      stage_time (total_timeout *. clamp_fraction fraction)
    in

    let run_legacy_then_modern () =
      let size_threshold = getenv_int "VIP_PORTFOLIO_SIZE_THRESHOLD" 160 in
      let modern_biased_small =
        clause_count <= getenv_int "VIP_PORTFOLIO_TINY_MODERN_MAX_CLAUSES" 4
        || ((clause_count >= getenv_int "VIP_PORTFOLIO_MEDIUM_MODERN_MIN_CLAUSES" 13
             && clause_count <= getenv_int "VIP_PORTFOLIO_MEDIUM_MODERN_MAX_CLAUSES" 24)
            && not (clause_count >= getenv_int "VIP_PORTFOLIO_MEDIUM_LEGACY_MIN_CLAUSES" 18
                    && clause_count <= getenv_int "VIP_PORTFOLIO_MEDIUM_LEGACY_MAX_CLAUSES" 20))
      in
      let modern_only_threshold = getenv_int "VIP_PORTFOLIO_MODERN_ONLY_MIN_CLAUSES" 80 in
      let default_flash_fraction, default_modern_fraction =
        if clause_count >= modern_only_threshold then
          (getenv_float "VIP_PORTFOLIO_LARGE_LEGACY_FLASH_FRACTION" 0.0,
           getenv_float "VIP_PORTFOLIO_LARGE_MODERN_FRACTION" 1.0)
        else if clause_count < size_threshold && modern_biased_small then
          (getenv_float "VIP_PORTFOLIO_SMALL_LEGACY_FRACTION" 0.05,
           getenv_float "VIP_PORTFOLIO_SMALL_MODERN_FRACTION" 0.95)
        else if clause_count < size_threshold then
          (getenv_float "VIP_PORTFOLIO_SMALL_LEGACY_FRACTION" 0.50,
           getenv_float "VIP_PORTFOLIO_SMALL_MODERN_FRACTION" 0.50)
        else
          (getenv_float "VIP_PORTFOLIO_LARGE_LEGACY_FLASH_FRACTION" 0.0,
           getenv_float "VIP_PORTFOLIO_LARGE_MODERN_FRACTION" 1.0)
      in
      let flash_fraction =
        match getenv_float_opt "VIP_PORTFOLIO_LEGACY_FLASH_FRACTION" with
        | Some f -> f
        | None -> default_flash_fraction
      in
      let modern_fraction =
        match getenv_float_opt "VIP_PORTFOLIO_MODERN_FRACTION" with
        | Some f -> f
        | None -> default_modern_fraction
      in
      let flash_budget = fraction_budget flash_fraction in
      let modern_budget = fraction_budget modern_fraction in
      let legacy_res =
        run_stage
          {
            stage_name = "Legacy compatibility flash";
            engine = Legacy_compat;
            time_limit_s = flash_budget;
          }
      in
      match legacy_res.stop_reason with
      | Refutation_found _ -> legacy_res
      | Saturation | Time_limit | Clause_limit ->
          let modern_res =
            let remaining_time = remaining_time () in
            if remaining_time <= 0.1 then legacy_res
            else
              run_stage
                {
                  stage_name = "Modern deep search";
                  engine = Modern_deep;
                  time_limit_s = min modern_budget remaining_time;
                }
          in
          match modern_res.stop_reason with
          | Refutation_found _ -> modern_res
          | Saturation | Time_limit | Clause_limit ->
              let remaining_time = remaining_time () in
              if remaining_time <= 0.1 then modern_res
              else
                run_stage
                  {
                    stage_name = "Legacy compatibility fallback";
                    engine = Legacy_compat;
                    time_limit_s = remaining_time;
                  }
    in

    let with_env name value f =
      let old = env_opt name in
      Option.iter (fun v -> Unix.putenv name v) value;
      Fun.protect
        ~finally:(fun () ->
          match old with
          | Some v -> Unix.putenv name v
          | None -> Unix.putenv name "")
        f
    in

    let with_envs bindings f =
      let rec loop bindings =
        match bindings with
        | [] -> f ()
        | (name, value) :: tl -> with_env name value (fun () -> loop tl)
      in
      loop bindings
    in

    let run_profile_stage ~stage_name ~engine ~fraction ?(env = []) () =
      let budget = fraction_budget fraction in
      if budget <= 0.05 then timeout_result (elapsed ())
      else with_envs env (fun () -> run_stage { stage_name; engine; time_limit_s = budget })
    in

    let run_remaining_stage ~stage_name ~engine ?(env = []) () =
      let budget = remaining_time () in
      if budget <= 0.1 then timeout_result (elapsed ())
      else with_envs env (fun () -> run_stage { stage_name; engine; time_limit_s = budget })
    in

    let run_schedule (stages : (unit -> run_result) list) =
      let rec loop last = function
        | [] -> last
        | _ when (match last.stop_reason with Refutation_found _ -> true | _ -> false) -> last
        | run :: tl ->
            let remaining = remaining_time () in
            if remaining <= 0.1 then last
            else
              let res = run () in
              match res.stop_reason with
              | Refutation_found _ -> res
              | Saturation | Time_limit | Clause_limit -> loop res tl
      in
      match stages with
      | [] -> timeout_result (elapsed ())
      | run :: tl -> loop (run ()) tl
    in

    let winning_tstp_prelude = ref None in

    let run_experimental_subrun ~stage_name ~portfolio_mode ~fraction
        ?(min_budget_s = 0.0) ?(env = []) () =
      if fraction <= 0.0 then
        timeout_result (elapsed ())
      else
        let budget =
          min (remaining_time ()) (max (fraction_budget fraction) min_budget_s)
        in
        if budget <= 0.1 then
          timeout_result (elapsed ())
        else begin
          if config.print_derivation then
            Printf.printf "%% Stage: %s (%.2fs, subrun)\n%!" stage_name budget;
          with_envs env (fun () ->
            let outcome =
              run_file
                ~config:
                  {
                    config with
                    time_limit_s = Some budget;
                    portfolio_mode;
                    print_derivation = false;
                  }
                filename
            in
            (match outcome.empty_clause with
             | Some _ -> winning_tstp_prelude := outcome.tstp_prelude
             | None -> ());
            let stop_reason =
              match outcome.empty_clause, outcome.status with
              | Some d, _ -> Refutation_found d
              | None, GaveUp -> Saturation
              | None, ResourceOut -> Clause_limit
              | None, Timeout -> Time_limit
              | None, _ -> Time_limit
            in
            {
              Resolution.stop_reason;
              derivation = outcome.derivation;
              stats =
              Option.value
                outcome.resolution_stats
                ~default:(empty_resolution_stats budget);
          })
      end
    in

    let run_experimental_casc () =
      let experimental_feq_legacy_flash =
        match env_opt "VIP_EXPERIMENTAL_FEQ_LEGACY_FLASH_FRACTION" with
        | Some value when String.trim value <> "" -> value
        | _ -> "0"
      in
      with_env
        "VIP_FEQ_LEGACY_FLASH_FRACTION"
        (Some experimental_feq_legacy_flash)
        (fun () ->
      let small_non_equality =
        problem_profile = Non_equality
        && raw_clause_count
           <= getenv_int "VIP_EXPERIMENTAL_SMALL_NON_EQ_MAX_CLAUSES" 200
        && raw_equality_literals = 0
      in
      let compact_non_unit_non_equality =
        small_non_equality
        && raw_clause_count
           <= getenv_int "VIP_EXPERIMENTAL_COMPACT_NON_UNIT_MAX_CLAUSES" 30
        && unit_ratio <= getenv_float "VIP_EXPERIMENTAL_COMPACT_NON_UNIT_MAX_UNIT_RATIO" 0.05
      in
      let avatar_fallback () =
        run_remaining_stage
          ~stage_name:"Experimental AVATAR fallback"
          ~engine:Modern_deep
          ~env:
            [
              "VIP_AVATAR_SPLITTING", Some "1";
              "VIP_AVATAR_GROUND_ONLY", Some "0";
              "VIP_AVATAR_KEEP_ORIGINAL", Some "1";
              "VIP_AVATAR_MIN_SPLIT", Some "4";
              "VIP_AVATAR_MAX_SPLIT_VARS", Some "8";
              "VIP_AVATAR_MAX_SPLIT_VARS_PER_CLAUSE", Some "3";
              "VIP_AVATAR_MODEL_FALSE_FIRST", Some "1";
            ]
          ()
      in
      let feq_subrun_env extra =
        let legacy_flash =
          match env_opt "VIP_EXPERIMENTAL_FEQ_LEGACY_FLASH_FRACTION" with
          | Some value when String.trim value <> "" -> value
          | _ -> "0"
        in
        ("VIP_FEQ_LEGACY_FLASH_FRACTION", Some legacy_flash) :: extra
      in
      let feq_standard_env extra =
        let legacy_flash =
          match env_opt "VIP_EXPERIMENTAL_STANDARD_FEQ_LEGACY_FLASH_FRACTION" with
          | Some value when String.trim value <> "" -> value
          | _ -> "0.05"
        in
        ("VIP_FEQ_LEGACY_FLASH_FRACTION", Some legacy_flash) :: extra
      in
      let large_general = problem_profile = Large_general in
      if compact_non_unit_non_equality then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental compact non-unit modern"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_COMPACT_NON_UNIT_MODERN_FRACTION"
                   0.85)
              ~env:[ "VIP_PASSIVE_SELECTION", Some "classic" ];
            run_experimental_subrun
              ~stage_name:"Experimental compact non-unit legacy-guided"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_COMPACT_NON_UNIT_LEGACY_GUIDED_FRACTION"
                   0.10)
              ~env:
                [
                  "VIP_PASSIVE_SELECTION", Some "legacy";
                  "VIP_LITERAL_SELECTION", Some "legacy";
                  "VIP_RESOLUTION_LITERAL_SELECTION", Some "legacy";
                  "VIP_LEGACY_SUBSUMPTION", Some "1";
                  "VIP_FAST_CONDENSATION", Some "0";
                  "VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "0";
                ];
            run_remaining_stage
              ~stage_name:"Experimental compact non-unit legacy fallback"
              ~engine:Legacy_compat;
          ]
      else if small_non_equality then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality legacy probe"
              ~portfolio_mode:Legacy_only
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_SMALL_LEGACY_PROBE_FRACTION"
                   0.50);
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality modern portfolio"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_SMALL_MODERN_PORTFOLIO_FRACTION"
                   0.25);
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality goal-directed"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_SMALL_GOAL_FRACTION"
                   0.0)
              ~env:
                [
                  "VIP_PASSIVE_SELECTION", Some "goal";
                  "VIP_LITERAL_SELECTION", Some "smallest-negative";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality modern legacy-guided"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_SMALL_LEGACY_GUIDED_FRACTION"
                   0.15)
              ~env:
                [
                  "VIP_PASSIVE_SELECTION", Some "legacy";
                  "VIP_LITERAL_SELECTION", Some "legacy";
                  "VIP_RESOLUTION_LITERAL_SELECTION", Some "legacy";
                  "VIP_LEGACY_SUBSUMPTION", Some "1";
                  "VIP_FAST_CONDENSATION", Some "0";
                  "VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "0";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality legacy fallback"
              ~portfolio_mode:Legacy_only
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_SMALL_LEGACY_FRACTION" 0.05);
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality modern classic"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_SMALL_MODERN_FRACTION" 0.10)
              ~env:[ "VIP_PASSIVE_SELECTION", Some "classic" ];
            run_remaining_stage
              ~stage_name:"Experimental small non-equality stable fallback"
              ~engine:Legacy_compat;
          ]
      else if large_general then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental large legacy probe"
              ~portfolio_mode:Legacy_only
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_LARGE_LEGACY_PROBE_FRACTION"
                   0.60);
            run_experimental_subrun
              ~stage_name:"Experimental large SInE narrow"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_SINE_NARROW_FRACTION" 0.05)
              ~env:
                (feq_subrun_env [
                  "VIP_AXIOM_SELECTION", Some "1";
                  "VIP_AXIOM_SELECTION_ALL", Some "1";
                  "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "300";
                  "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "128";
                  "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                ]);
            run_experimental_subrun
              ~stage_name:"Experimental large SInE medium"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_SINE_MEDIUM_FRACTION" 0.05)
              ~env:
                (feq_subrun_env [
                  "VIP_AXIOM_SELECTION", Some "1";
                  "VIP_AXIOM_SELECTION_ALL", Some "1";
                  "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "1000";
                  "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "256";
                  "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                ]);
            run_experimental_subrun
              ~stage_name:"Experimental large goal-directed SInE"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_LARGE_GOAL_SINE_FRACTION" 0.0)
              ~env:
                [
                  "VIP_AXIOM_SELECTION", Some "1";
                  "VIP_AXIOM_SELECTION_ALL", Some "1";
                  "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "1000";
                  "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "256";
                  "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                  "VIP_PASSIVE_SELECTION", Some "goal";
                  "VIP_LITERAL_SELECTION", Some "smallest-negative";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental large unselected first"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_LARGE_FULL_FIRST_FRACTION" 0.40)
              ~env:
                (feq_subrun_env [
                  "VIP_AXIOM_SELECTION", Some "0";
                  "VIP_AXIOM_SELECTION_ALL", Some "0";
                ]);
            run_experimental_subrun
              ~stage_name:"Experimental large layered modern"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_LARGE_LAYERED_FRACTION" 0.0)
              ~env:
                [
                  "VIP_PASSIVE_SELECTION", Some "layered";
                  "VIP_LAYERED_SELECTION",
                  Some "unit,equality,goal,short,age,weight";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental large SInE wide"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "VIP_EXPERIMENTAL_SINE_WIDE_FRACTION" 0.05)
              ~env:
                (feq_subrun_env [
                  "VIP_AXIOM_SELECTION", Some "1";
                  "VIP_AXIOM_SELECTION_ALL", Some "1";
                  "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "2500";
                  "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "512";
                  "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                ]);
          ]
      else if problem_profile = Equality_heavy
              && raw_clause_count
                 >= getenv_int "VIP_EXPERIMENTAL_EQ_HEAVY_SINE_MIN_CLAUSES" 500 then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental equality-heavy SInE narrow"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQ_HEAVY_SINE_FRACTION"
                   0.25)
              ~min_budget_s:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQ_HEAVY_SINE_MIN_SECONDS"
                   4.2)
              ~env:
                (feq_subrun_env [
                  "VIP_AXIOM_SELECTION", Some "1";
                  "VIP_AXIOM_SELECTION_ALL", Some "1";
                  "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "300";
                  "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "128";
                  "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                ]);
            run_experimental_subrun
              ~stage_name:"Experimental equality-heavy full fallback"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQ_HEAVY_FULL_FRACTION"
                   0.50)
              ~env:
                (feq_subrun_env [
                  "VIP_AXIOM_SELECTION", Some "0";
                  "VIP_AXIOM_SELECTION_ALL", Some "0";
                ]);
            (fun () -> avatar_fallback ());
          ]
      else if problem_profile = Equality_light then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental equality-light FEQ equality probe"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQUALITY_LIGHT_FEQ_EQUALITY_PROBE_FRACTION"
                   0.10)
              ~min_budget_s:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQUALITY_LIGHT_FEQ_EQUALITY_PROBE_MIN_SECONDS"
                   4.0)
              ~env:
                (feq_subrun_env
                   [
                     "VIP_FEQ_LEGACY_FLASH_FRACTION", Some "0";
                     "VIP_FEQ_MODERN_ALL", Some "1";
                     "VIP_FEQ_STABLE_FRACTION", Some "0.05";
                     "VIP_FEQ_CLASSIC_FRACTION", Some "0.15";
                     "VIP_FEQ_WEIGHT_FRACTION", Some "0.15";
                     "VIP_FEQ_EQUALITY_FRACTION", Some "0.60";
                     "VIP_FEQ_PASSIVE_AW_RATIO", Some "1:8";
                   ]);
            run_experimental_subrun
              ~stage_name:"Experimental equality-light FEQ standard"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQUALITY_LIGHT_FEQ_STANDARD_FRACTION"
                   0.35)
              ~env:(feq_standard_env []);
            run_experimental_subrun
              ~stage_name:"Experimental equality-light FEQ full schedule"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "VIP_EXPERIMENTAL_EQUALITY_LIGHT_FEQ_ALL_FRACTION"
                   0.35)
              ~env:(feq_subrun_env [ "VIP_FEQ_MODERN_ALL", Some "1" ]);
            (fun () -> avatar_fallback ());
          ]
      else
        run_experimental_subrun
          ~stage_name:"Experimental stable fallback"
          ~portfolio_mode:Feq_modern
          ~fraction:(getenv_float "VIP_EXPERIMENTAL_STABLE_FRACTION" 0.70)
          ~env:(feq_standard_env [])
          ()
        |> fun first ->
        match first.stop_reason with
        | Refutation_found _ -> first
        | Saturation | Time_limit | Clause_limit ->
            avatar_fallback ()
        )
    in

    let run_casc_aggressive () =
      let feq_env extra =
        ("VIP_FEQ_LEGACY_FLASH_FRACTION", Some "0") :: extra
      in
      let avatar_env =
        [
          "VIP_AVATAR_SPLITTING", Some "1";
          "VIP_AVATAR_GROUND_ONLY", Some "0";
          "VIP_AVATAR_KEEP_ORIGINAL", Some "1";
          "VIP_AVATAR_MIN_SPLIT", Some "3";
          "VIP_AVATAR_MAX_SPLIT_VARS", Some "10";
          "VIP_AVATAR_MAX_SPLIT_VARS_PER_CLAUSE", Some "4";
          "VIP_AVATAR_MODEL_FALSE_FIRST", Some "1";
        ]
      in
      let legacy_guided_env =
        [
          "VIP_PASSIVE_SELECTION", Some "legacy";
          "VIP_LITERAL_SELECTION", Some "legacy";
          "VIP_RESOLUTION_LITERAL_SELECTION", Some "legacy";
          "VIP_LEGACY_SUBSUMPTION", Some "1";
          "VIP_FAST_CONDENSATION", Some "0";
          "VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "0";
        ]
      in
      let feq_sine name fraction max_axioms max_freq =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Feq_modern
          ~fraction
          ~env:
            (feq_env
               [
                 "VIP_FEQ_MODERN_ALL", Some "1";
                 "VIP_AXIOM_SELECTION", Some "1";
                 "VIP_AXIOM_SELECTION_ALL", Some "1";
                 "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some max_axioms;
                 "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some max_freq;
                 "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                 "VIP_FEQ_STABLE_FRACTION", Some "0.10";
                 "VIP_FEQ_CLASSIC_FRACTION", Some "0.45";
                 "VIP_FEQ_WEIGHT_FRACTION", Some "0.20";
                 "VIP_FEQ_EQUALITY_FRACTION", Some "0.20";
               ])
      in
      let avatar_stage fraction =
        run_experimental_subrun
          ~stage_name:"Aggressive AVATAR stage"
          ~portfolio_mode:Modern_only
          ~fraction
          ~env:avatar_env
      in
      let definitional_stage fraction =
        run_experimental_subrun
          ~stage_name:"Aggressive definitional CNF FEQ"
          ~portfolio_mode:Feq_modern
          ~fraction
          ~env:
            (feq_env
               [
                 "VIP_AUTO_DEFINITIONAL_CNF", Some "1";
                 "VIP_CNF_DISTRIBUTION_LIMIT", Some "4096";
                 "VIP_FEQ_MODERN_ALL", Some "1";
                 "VIP_FEQ_STABLE_FRACTION", Some "0.05";
                 "VIP_FEQ_CLASSIC_FRACTION", Some "0.45";
                 "VIP_FEQ_WEIGHT_FRACTION", Some "0.25";
                 "VIP_FEQ_EQUALITY_FRACTION", Some "0.20";
               ])
      in
      let legacy_probe fraction =
        run_experimental_subrun
          ~stage_name:"Aggressive legacy probe"
          ~portfolio_mode:Legacy_only
          ~fraction
      in
      let legacy_guided fraction =
        run_experimental_subrun
          ~stage_name:"Aggressive legacy-guided modern"
          ~portfolio_mode:Modern_only
          ~fraction
          ~env:legacy_guided_env
      in
      let stable_first =
        run_experimental_subrun
          ~stage_name:"Aggressive stable first pass"
          ~portfolio_mode:Experimental_casc
          ~fraction:(getenv_float "VIP_AGGRESSIVE_STABLE_FIRST_FRACTION" 0.75)
          ~env:[ "VIP_EXPERIMENTAL_FEQ_LEGACY_FLASH_FRACTION", Some "0" ]
          ()
      in
      match stable_first.stop_reason with
      | Refutation_found _ -> stable_first
      | Saturation | Time_limit | Clause_limit ->
      if problem_profile = Equality_heavy || problem_profile = Equality_light then
        run_schedule
          [
            feq_sine
              "Aggressive FEQ SInE medium"
              (getenv_float "VIP_AGGRESSIVE_FEQ_SINE_MEDIUM_FRACTION" 0.16)
              "1200"
              "256";
            avatar_stage
              (getenv_float "VIP_AGGRESSIVE_FEQ_AVATAR_FRACTION" 0.12);
            feq_sine
              "Aggressive FEQ SInE wide"
              (getenv_float "VIP_AGGRESSIVE_FEQ_SINE_WIDE_FRACTION" 0.10)
              "3000"
              "768";
            definitional_stage
              (getenv_float "VIP_AGGRESSIVE_FEQ_DEFINITIONAL_FRACTION" 0.06);
            run_remaining_stage
              ~stage_name:"Aggressive FEQ remaining AVATAR"
              ~engine:Modern_deep
              ~env:avatar_env;
          ]
      else if problem_profile = Large_general then
        run_schedule
          [
            legacy_probe
              (getenv_float "VIP_AGGRESSIVE_LARGE_LEGACY_FRACTION" 0.35);
            run_experimental_subrun
              ~stage_name:"Aggressive large full modern"
              ~portfolio_mode:Feq_modern
              ~fraction:(getenv_float "VIP_AGGRESSIVE_LARGE_FULL_FRACTION" 0.25)
              ~env:
                (feq_env
                   [
                     "VIP_AXIOM_SELECTION", Some "0";
                     "VIP_AXIOM_SELECTION_ALL", Some "0";
                   ]);
            legacy_guided
              (getenv_float "VIP_AGGRESSIVE_LARGE_LEGACY_GUIDED_FRACTION" 0.18);
            feq_sine
              "Aggressive large SInE wide"
              (getenv_float "VIP_AGGRESSIVE_LARGE_SINE_FRACTION" 0.15)
              "3000"
              "768";
            run_remaining_stage
              ~stage_name:"Aggressive large remaining legacy"
              ~engine:Legacy_compat;
          ]
      else
        run_schedule
          [
            legacy_probe
              (getenv_float "VIP_AGGRESSIVE_GENERAL_LEGACY_FRACTION" 0.45);
            legacy_guided
              (getenv_float "VIP_AGGRESSIVE_GENERAL_LEGACY_GUIDED_FRACTION" 0.25);
            run_experimental_subrun
              ~stage_name:"Aggressive general modern classic"
              ~portfolio_mode:Modern_only
              ~fraction:(getenv_float "VIP_AGGRESSIVE_GENERAL_CLASSIC_FRACTION" 0.15)
              ~env:[ "VIP_PASSIVE_SELECTION", Some "classic" ];
            avatar_stage
              (getenv_float "VIP_AGGRESSIVE_GENERAL_AVATAR_FRACTION" 0.10);
            run_remaining_stage
              ~stage_name:"Aggressive general remaining legacy"
              ~engine:Legacy_compat;
          ]
    in

    let run_casc_240 ?(extended = false) () =
      let feq_env extra =
        ("VIP_FEQ_LEGACY_FLASH_FRACTION", Some "0") :: extra
      in
      let avatar_env =
        [
          "VIP_AVATAR_SPLITTING", Some "1";
          "VIP_AVATAR_GROUND_ONLY", Some "0";
          "VIP_AVATAR_KEEP_ORIGINAL", Some "1";
          "VIP_AVATAR_MIN_SPLIT", Some "3";
          "VIP_AVATAR_MAX_SPLIT_VARS", Some "10";
          "VIP_AVATAR_MAX_SPLIT_VARS_PER_CLAUSE", Some "4";
          "VIP_AVATAR_MODEL_FALSE_FIRST", Some "1";
        ]
      in
      let legacy_guided_env =
        [
          "VIP_PASSIVE_SELECTION", Some "legacy";
          "VIP_LITERAL_SELECTION", Some "legacy";
          "VIP_RESOLUTION_LITERAL_SELECTION", Some "legacy";
          "VIP_LEGACY_SUBSUMPTION", Some "1";
          "VIP_FAST_CONDENSATION", Some "0";
          "VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "0";
        ]
      in
      let feq_sine name fraction max_axioms max_freq =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Feq_modern
          ~fraction
          ~env:
            (feq_env
               [
                 "VIP_FEQ_MODERN_ALL", Some "1";
                 "VIP_AXIOM_SELECTION", Some "1";
                 "VIP_AXIOM_SELECTION_ALL", Some "1";
                 "VIP_AXIOM_SELECTION_RANKED", Some "1";
                 "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some max_axioms;
                 "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some max_freq;
                 "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                 "VIP_FEQ_STABLE_FRACTION", Some "0.08";
                 "VIP_FEQ_CLASSIC_FRACTION", Some "0.42";
                 "VIP_FEQ_WEIGHT_FRACTION", Some "0.18";
                 "VIP_FEQ_EQUALITY_FRACTION", Some "0.27";
               ])
      in
      let feq_sine_compat name fraction max_axioms max_freq =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Feq_modern
          ~fraction
          ~min_budget_s:
            (getenv_float "VIP_CASC_240_FEQ_SINE_NARROW_MIN_SECONDS" 8.0)
          ~env:
            (feq_env
               [
                 "VIP_AXIOM_SELECTION", Some "1";
                 "VIP_AXIOM_SELECTION_ALL", Some "1";
                 "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some max_axioms;
                 "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some max_freq;
                 "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
               ])
      in
      let feq_simplification_probe name fraction =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Feq_modern
          ~fraction
          ~min_budget_s:
            (getenv_float "VIP_CASC_150_FEQ_SIMPL_MIN_SECONDS" 5.0)
          ~env:
            (feq_env
               [
                 "VIP_FEQ_MODERN_ALL", Some "1";
                 "VIP_AXIOM_SELECTION", Some "0";
                 "VIP_AXIOM_SELECTION_ALL", Some "0";
                 "VIP_SIMPLIFICATION_SET_INDEX", Some "1";
                 "VIP_SIMPLIFICATION_SET_UNIT_ONLY", Some "0";
                 "VIP_SIMPLIFICATION_SET_MAX_CLAUSE_LEN", Some "6";
                 "VIP_SIMPLIFICATION_SET_NEGATIVE_MAX_LEN", Some "4";
                 "VIP_SIMPLIFICATION_SET_GOAL_MAX_LEN", Some "6";
                 "VIP_SIMPLIFICATION_SET_EQUALITY_UNIT", Some "1";
                 "VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "1";
               ])
      in
      let legacy_guided_probe name fraction =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Modern_only
          ~fraction
          ~env:legacy_guided_env
      in
      let fne_layered_guard name fraction =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Modern_only
          ~fraction
          ~min_budget_s:
            (getenv_float "VIP_CASC_150_FNE_LAYERED_GUARD_MIN_SECONDS" 6.0)
          ~env:
            [
              "VIP_PASSIVE_SELECTION", Some "layered";
              "VIP_LAYERED_SELECTION",
              Some "unit,equality,goal,short,age,weight";
            ]
      in
      let with_fne_layered_guard name fraction fallback =
        let min_raw_clauses =
          getenv_int_global "VIP_CASC_150_FNE_LAYERED_GUARD_MIN_RAW_CLAUSES" 50
        in
        let max_raw_clauses =
          getenv_int_global "VIP_CASC_150_FNE_LAYERED_GUARD_MAX_RAW_CLAUSES" 499
        in
        if
          extended
          && fraction > 0.0
          && raw_clause_count >= min_raw_clauses
          && raw_clause_count <= max_raw_clauses
        then
          let res = fne_layered_guard name fraction () in
          match res.stop_reason with
          | Refutation_found _ -> res
          | Saturation | Time_limit | Clause_limit -> fallback ()
        else
          fallback ()
      in
      let fne_layered_guard_stage name fraction () =
        with_fne_layered_guard name fraction (fun () -> timeout_result (elapsed ()))
      in
      let feq_full_with_min min_budget_s name fraction =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Feq_modern
          ~fraction
          ~min_budget_s
          ~env:
            (feq_env
               [
                 "VIP_FEQ_MODERN_ALL", Some "1";
                 "VIP_AXIOM_SELECTION", Some "0";
                 "VIP_AXIOM_SELECTION_ALL", Some "0";
                 "VIP_FEQ_STABLE_FRACTION", Some "0.08";
                 "VIP_FEQ_CLASSIC_FRACTION", Some "0.40";
                 "VIP_FEQ_WEIGHT_FRACTION", Some "0.20";
	                 "VIP_FEQ_EQUALITY_FRACTION", Some "0.27";
	               ])
      in
      let feq_full name fraction = feq_full_with_min 0.0 name fraction in
      let definitional_feq fraction =
        run_experimental_subrun
          ~stage_name:"CASC-240 definitional FEQ"
          ~portfolio_mode:Feq_modern
          ~fraction
          ~env:
            (feq_env
               [
                 "VIP_AUTO_DEFINITIONAL_CNF", Some "1";
                 "VIP_CNF_DISTRIBUTION_LIMIT", Some "4096";
                 "VIP_FEQ_MODERN_ALL", Some "1";
                 "VIP_FEQ_STABLE_FRACTION", Some "0.05";
                 "VIP_FEQ_CLASSIC_FRACTION", Some "0.45";
                 "VIP_FEQ_WEIGHT_FRACTION", Some "0.20";
                 "VIP_FEQ_EQUALITY_FRACTION", Some "0.25";
               ])
      in
      let avatar_stage name fraction =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Modern_only
          ~fraction
          ~env:avatar_env
      in
      let stable_stage name fraction =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Experimental_casc
          ~fraction
          ~min_budget_s:(getenv_float "VIP_CASC_240_STABLE_MIN_SECONDS" 12.0)
          ~env:
            [
              "VIP_EXPERIMENTAL_FEQ_LEGACY_FLASH_FRACTION", Some "0";
              "VIP_SIMPLIFICATION_SET_INDEX", Some "0";
            ]
      in
      let stable_first fraction =
        let res =
          stable_stage "CASC-240 stable experimental pass" fraction ()
        in
        match res.stop_reason with
        | Refutation_found _ -> Some res
        | Saturation | Time_limit | Clause_limit -> None
      in
      if problem_profile = Equality_heavy then
            run_schedule
              ([
                (fun () -> feq_full_with_min
                  (getenv_float "VIP_CASC_240_FEQ_QUICK_FULL_MIN_SECONDS" 8.0)
                  "CASC-240 FEQ quick full classic/equality"
                  (getenv_float "VIP_CASC_240_FEQ_QUICK_FULL_FRACTION" 0.10) ());
                (fun () -> feq_sine_compat
                  "CASC-240 FEQ ranked SInE narrow"
                  (getenv_float "VIP_CASC_240_FEQ_SINE_NARROW_FRACTION" 0.04)
                  "300"
                  "128" ());
                (fun () -> stable_stage
                  "CASC-240 FEQ stable pass"
                  (getenv_float "VIP_CASC_240_FEQ_STABLE_FRACTION" 0.41) ());
                (fun () -> feq_sine
                  "CASC-240 FEQ ranked SInE medium"
                  (getenv_float "VIP_CASC_240_FEQ_SINE_MEDIUM_FRACTION" 0.14)
                  "1200"
                  "256" ());
                (fun () -> feq_full
                  "CASC-240 FEQ full classic/equality"
                  (getenv_float "VIP_CASC_240_FEQ_FULL_FRACTION" 0.06) ());
                (fun () -> feq_sine
                  "CASC-240 FEQ ranked SInE wide"
                  (getenv_float "VIP_CASC_240_FEQ_SINE_WIDE_FRACTION" 0.06)
                  "3000"
                  "768" ());
              ]
              @
              (if extended then
                 [
                   (fun () -> feq_sine
                     "CASC-150 FEQ ranked SInE extra wide"
                     (getenv_float
                        "VIP_CASC_150_FEQ_SINE_EXTRA_WIDE_FRACTION"
                        0.04)
                     "5000"
                     "1024" ());
                   (fun () -> feq_simplification_probe
                     "CASC-150 FEQ simplification-set probe"
                     (getenv_float "VIP_CASC_150_FEQ_SIMPL_FRACTION" 0.025) ());
                   (fun () -> legacy_guided_probe
                     "CASC-150 FEQ legacy-guided probe"
                     (getenv_float
                        "VIP_CASC_150_FEQ_LEGACY_GUIDED_FRACTION"
                        0.025) ());
                 ]
               else [])
              @
              [
                (fun () -> definitional_feq
                  (getenv_float "VIP_CASC_240_FEQ_DEFINITIONAL_FRACTION" 0.04) ());
                (fun () -> avatar_stage
                  "CASC-240 FEQ AVATAR"
                  (getenv_float "VIP_CASC_240_FEQ_AVATAR_FRACTION" 0.04) ());
                (fun () -> run_remaining_stage
                  ~stage_name:"CASC-240 FEQ remaining full"
                  ~engine:Modern_feq
                  ~env:
                    (feq_env
                       [
                         "VIP_FEQ_MODERN_ALL", Some "1";
                         "VIP_PASSIVE_SELECTION", Some "equality";
                         "VIP_PASSIVE_AW_RATIO", Some "1:8";
                       ]) ());
              ])
      else if problem_profile = Equality_light then
            run_schedule
              ([
                (fun () -> feq_full
                  "CASC-240 equality-light quick full FEQ"
                  (getenv_float "VIP_CASC_240_EQ_LIGHT_QUICK_FULL_FRACTION" 0.12) ());
                (fun () -> stable_stage
                  "CASC-240 equality-light stable pass"
                  (getenv_float "VIP_CASC_240_EQ_LIGHT_STABLE_FRACTION" 0.40) ());
                (fun () -> feq_full
                  "CASC-240 equality-light full FEQ"
                  (getenv_float "VIP_CASC_240_EQ_LIGHT_FULL_FRACTION" 0.08) ());
                (fun () -> feq_sine
                  "CASC-240 equality-light SInE medium"
                  (getenv_float "VIP_CASC_240_EQ_LIGHT_SINE_FRACTION" 0.10)
                  "1200"
                  "256" ());
                (fun () -> avatar_stage
                  "CASC-240 equality-light AVATAR"
                  (getenv_float "VIP_CASC_240_EQ_LIGHT_AVATAR_FRACTION" 0.06) ());
                (fun () -> definitional_feq
                  (getenv_float "VIP_CASC_240_EQ_LIGHT_DEFINITIONAL_FRACTION" 0.05) ());
              ]
              @
              (if extended then
                 [
                   (fun () -> feq_sine
                     "CASC-150 equality-light SInE wide"
                     (getenv_float
                        "VIP_CASC_150_EQ_LIGHT_SINE_WIDE_FRACTION"
                        0.05)
                     "3000"
                     "768" ());
                   (fun () -> feq_simplification_probe
                     "CASC-150 equality-light simplification-set probe"
                     (getenv_float
                        "VIP_CASC_150_EQ_LIGHT_SIMPL_FRACTION"
                        0.04) ());
                 ]
               else [])
              @
              [
                (fun () -> run_remaining_stage
                  ~stage_name:"CASC-240 equality-light remaining FEQ"
                  ~engine:Modern_feq
                  ~env:
                    (feq_env
                       [
                         "VIP_FEQ_MODERN_ALL", Some "1";
                         "VIP_PASSIVE_SELECTION", Some "classic";
                         "VIP_PASSIVE_AW_RATIO", Some "1:6";
                       ]) ());
              ])
      else if problem_profile = Large_general then
        with_fne_layered_guard
          "CASC-150 large early layered guard"
          (getenv_float "VIP_CASC_150_LARGE_EARLY_LAYERED_FRACTION" 0.0)
          (fun () ->
        match stable_first (getenv_float "VIP_CASC_240_LARGE_STABLE_FRACTION" 0.45) with
        | Some res -> res
        | None ->
            run_schedule
              ([
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 large legacy probe"
                  ~portfolio_mode:Legacy_only
                  ~fraction:
                    (getenv_float "VIP_CASC_240_LARGE_LEGACY_FRACTION" 0.22) ());
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 large full modern"
                  ~portfolio_mode:Feq_modern
                  ~fraction:
                    (getenv_float "VIP_CASC_240_LARGE_FULL_FRACTION" 0.12)
                  ~env:
                    (feq_env
                       [
	                         "VIP_AXIOM_SELECTION", Some "0";
	                         "VIP_AXIOM_SELECTION_ALL", Some "0";
	                       ]) ());
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 large ranked SInE"
                  ~portfolio_mode:Feq_modern
                  ~fraction:
                    (getenv_float "VIP_CASC_240_LARGE_SINE_FRACTION" 0.10)
                  ~env:
                    (feq_env
                       [
                         "VIP_FEQ_MODERN_ALL", Some "1";
                         "VIP_AXIOM_SELECTION", Some "1";
                         "VIP_AXIOM_SELECTION_ALL", Some "1";
                         "VIP_AXIOM_SELECTION_RANKED", Some "1";
                         "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "2500";
	                         "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "512";
	                         "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
	                       ]) ());
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 large legacy-guided modern"
                  ~portfolio_mode:Modern_only
                  ~fraction:
                    (getenv_float
	                       "VIP_CASC_240_LARGE_LEGACY_GUIDED_FRACTION"
	                       0.08)
                  ~env:legacy_guided_env ());
              ]
              @
              (if extended then
                 [
                   (fun () -> run_experimental_subrun
                     ~stage_name:"CASC-150 large layered/goal probe"
                     ~portfolio_mode:Modern_only
                     ~fraction:
                       (getenv_float
                          "VIP_CASC_150_LARGE_LAYERED_GOAL_FRACTION"
                          0.03)
                     ~env:
                       [
                         "VIP_PASSIVE_SELECTION", Some "layered";
	                         "VIP_LAYERED_SELECTION",
	                         Some "unit,equality,goal,short,age,weight";
                         "VIP_LITERAL_SELECTION", Some "smallest-negative";
	                       ] ());
                 ]
               else [])
              @
              [
                (fun () -> run_remaining_stage
                  ~stage_name:"CASC-240 large remaining legacy"
                  ~engine:Legacy_compat ());
              ]))
      else
        match stable_first (getenv_float "VIP_CASC_240_NON_EQ_STABLE_FRACTION" 0.45) with
        | Some res -> res
        | None ->
            run_schedule
              ([
                (fun () -> fne_layered_guard_stage
                  "CASC-150 non-equality medium layered guard"
                  (getenv_float "VIP_CASC_150_NON_EQ_LAYERED_GUARD_FRACTION" 0.05) ());
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 non-equality legacy probe"
                  ~portfolio_mode:Legacy_only
                  ~fraction:
                    (getenv_float "VIP_CASC_240_NON_EQ_LEGACY_FRACTION" 0.25) ());
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 non-equality legacy-guided modern"
                  ~portfolio_mode:Modern_only
                  ~fraction:
                    (getenv_float
	                       "VIP_CASC_240_NON_EQ_LEGACY_GUIDED_FRACTION"
	                       0.12)
                  ~env:legacy_guided_env ());
                (fun () -> run_experimental_subrun
                  ~stage_name:"CASC-240 non-equality layered modern"
                  ~portfolio_mode:Modern_only
                  ~fraction:
                    (getenv_float "VIP_CASC_240_NON_EQ_LAYERED_FRACTION" 0.08)
                  ~env:
                    [
	                      "VIP_PASSIVE_SELECTION", Some "layered";
	                      "VIP_LAYERED_SELECTION",
	                      Some "unit,goal,short,age,weight";
	                    ] ());
                (fun () -> avatar_stage
                  "CASC-240 non-equality AVATAR"
                  (getenv_float "VIP_CASC_240_NON_EQ_AVATAR_FRACTION" 0.05) ());
              ]
              @
              (if extended then
                 [
                   (fun () -> run_experimental_subrun
                     ~stage_name:"CASC-150 non-equality FNE legacy-guided probe"
                     ~portfolio_mode:Modern_only
                     ~fraction:
                       (getenv_float
	                          "VIP_CASC_150_NON_EQ_LEGACY_GUIDED_FRACTION"
	                          0.03)
                     ~env:legacy_guided_env ());
                 ]
               else [])
              @
              [
                (fun () -> run_remaining_stage
                  ~stage_name:"CASC-240 non-equality remaining legacy"
                  ~engine:Legacy_compat ());
              ])
    in

    let run_casc_feq_probe () =
      let feq_env extra =
        ("VIP_FEQ_LEGACY_FLASH_FRACTION", Some "0") :: extra
      in
      let stable_stage fraction =
        run_experimental_subrun
          ~stage_name:"FEQ-probe stable guard"
          ~portfolio_mode:Experimental_casc
          ~fraction
          ~min_budget_s:(getenv_float "VIP_CASC_FEQ_PROBE_STABLE_MIN_SECONDS" 12.0)
          ~env:
            [
              "VIP_EXPERIMENTAL_FEQ_LEGACY_FLASH_FRACTION", Some "0";
              "VIP_SIMPLIFICATION_SET_INDEX", Some "0";
            ]
      in
      let feq_stage name fraction env =
        run_experimental_subrun
          ~stage_name:name
          ~portfolio_mode:Feq_modern
          ~fraction
          ~env:(feq_env env)
      in
      let avatar_stage fraction =
        run_experimental_subrun
          ~stage_name:"FEQ-probe AVATAR"
          ~portfolio_mode:Modern_only
          ~fraction
          ~env:
            [
              "VIP_AVATAR_SPLITTING", Some "1";
              "VIP_AVATAR_GROUND_ONLY", Some "0";
              "VIP_AVATAR_KEEP_ORIGINAL", Some "1";
              "VIP_AVATAR_MIN_SPLIT", Some "3";
              "VIP_AVATAR_MAX_SPLIT_VARS", Some "10";
              "VIP_AVATAR_MAX_SPLIT_VARS_PER_CLAUSE", Some "4";
              "VIP_AVATAR_MODEL_FALSE_FIRST", Some "1";
              "VIP_PASSIVE_SELECTION", Some "equality";
            ]
      in
      if problem_profile <> Equality_heavy && problem_profile <> Equality_light then
        run_experimental_casc ()
      else
        run_schedule
          [
            stable_stage
              (getenv_float "VIP_CASC_FEQ_PROBE_STABLE_FRACTION" 0.30);
            feq_stage
              "FEQ-probe full equality-heavy"
              (getenv_float "VIP_CASC_FEQ_PROBE_FULL_FRACTION" 0.18)
              [
                "VIP_FEQ_MODERN_ALL", Some "1";
                "VIP_AXIOM_SELECTION", Some "0";
                "VIP_AXIOM_SELECTION_ALL", Some "0";
                "VIP_FEQ_STABLE_FRACTION", Some "0.05";
                "VIP_FEQ_CLASSIC_FRACTION", Some "0.28";
                "VIP_FEQ_WEIGHT_FRACTION", Some "0.18";
                "VIP_FEQ_EQUALITY_FRACTION", Some "0.44";
                "VIP_FEQ_PASSIVE_AW_RATIO", Some "1:10";
              ];
            feq_stage
              "FEQ-probe ranked SInE medium"
              (getenv_float "VIP_CASC_FEQ_PROBE_SINE_MEDIUM_FRACTION" 0.15)
              [
                "VIP_FEQ_MODERN_ALL", Some "1";
                "VIP_AXIOM_SELECTION", Some "1";
                "VIP_AXIOM_SELECTION_ALL", Some "1";
                "VIP_AXIOM_SELECTION_RANKED", Some "1";
                "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "1500";
                "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "256";
                "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                "VIP_FEQ_STABLE_FRACTION", Some "0.05";
                "VIP_FEQ_CLASSIC_FRACTION", Some "0.35";
                "VIP_FEQ_WEIGHT_FRACTION", Some "0.15";
                "VIP_FEQ_EQUALITY_FRACTION", Some "0.40";
                "VIP_FEQ_PASSIVE_AW_RATIO", Some "1:10";
              ];
            feq_stage
              "FEQ-probe ranked SInE wide"
              (getenv_float "VIP_CASC_FEQ_PROBE_SINE_WIDE_FRACTION" 0.12)
              [
                "VIP_FEQ_MODERN_ALL", Some "1";
                "VIP_AXIOM_SELECTION", Some "1";
                "VIP_AXIOM_SELECTION_ALL", Some "1";
                "VIP_AXIOM_SELECTION_RANKED", Some "1";
                "VIP_AXIOM_SELECTION_MAX_AXIOMS", Some "4000";
                "VIP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "1024";
                "VIP_AXIOM_SELECTION_SEED_PREDICATES_ONLY", Some "1";
                "VIP_FEQ_STABLE_FRACTION", Some "0.03";
                "VIP_FEQ_CLASSIC_FRACTION", Some "0.32";
                "VIP_FEQ_WEIGHT_FRACTION", Some "0.15";
                "VIP_FEQ_EQUALITY_FRACTION", Some "0.45";
                "VIP_FEQ_PASSIVE_AW_RATIO", Some "1:12";
              ];
            feq_stage
              "FEQ-probe definitional equality"
              (getenv_float "VIP_CASC_FEQ_PROBE_DEFINITIONAL_FRACTION" 0.08)
              [
                "VIP_AUTO_DEFINITIONAL_CNF", Some "1";
                "VIP_CNF_DISTRIBUTION_LIMIT", Some "4096";
                "VIP_FEQ_MODERN_ALL", Some "1";
                "VIP_FEQ_STABLE_FRACTION", Some "0.03";
                "VIP_FEQ_CLASSIC_FRACTION", Some "0.35";
                "VIP_FEQ_WEIGHT_FRACTION", Some "0.17";
                "VIP_FEQ_EQUALITY_FRACTION", Some "0.40";
                "VIP_FEQ_PASSIVE_AW_RATIO", Some "1:10";
              ];
            avatar_stage
              (getenv_float "VIP_CASC_FEQ_PROBE_AVATAR_FRACTION" 0.05);
            run_remaining_stage
              ~stage_name:"FEQ-probe remaining equality"
              ~engine:Modern_feq
              ~env:
                (feq_env
                   [
                     "VIP_FEQ_MODERN_ALL", Some "1";
                     "VIP_PASSIVE_SELECTION", Some "equality";
                     "VIP_PASSIVE_AW_RATIO", Some "1:12";
                     "VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "1";
                   ]);
          ]
    in

    let run_scheduled_portfolio () =
      let size_threshold = getenv_int "VIP_PORTFOLIO_SIZE_THRESHOLD" 160 in
      let modern_biased_small =
        clause_count <= getenv_int "VIP_PORTFOLIO_TINY_MODERN_MAX_CLAUSES" 4
        || ((clause_count >= getenv_int "VIP_PORTFOLIO_MEDIUM_MODERN_MIN_CLAUSES" 13
             && clause_count <= getenv_int "VIP_PORTFOLIO_MEDIUM_MODERN_MAX_CLAUSES" 24)
            && not (clause_count >= getenv_int "VIP_PORTFOLIO_MEDIUM_LEGACY_MIN_CLAUSES" 18
                    && clause_count <= getenv_int "VIP_PORTFOLIO_MEDIUM_LEGACY_MAX_CLAUSES" 20))
      in
      let syn_min = getenv_int "VIP_PASSIVE_SYN_MIN_CLAUSES" 40 in
      let syn_max = getenv_int "VIP_PASSIVE_SYN_MAX_CLAUSES" 55 in
      let syn_shaped = clause_count >= syn_min && clause_count <= syn_max in
      let legacy_sensitive_small =
        clause_count > getenv_int "VIP_PORTFOLIO_TINY_MODERN_MAX_CLAUSES" 4
        && clause_count < getenv_int "VIP_PORTFOLIO_LEGACY_ONLY_SMALL_MAX_CLAUSES" 13
      in
      let unit_heavy =
        unit_ratio >= getenv_float "VIP_PORTFOLIO_UNIT_HEAVY_RATIO" 0.55
      in
      let negative_heavy =
        negative_ratio >= getenv_float "VIP_PORTFOLIO_NEGATIVE_HEAVY_RATIO" 0.65
      in
      let term_heavy =
        equality_problem
        || avg_literal_term_size >= getenv_float "VIP_PORTFOLIO_TERM_HEAVY_AVG" 6.0
      in
      let legacy_stage fraction =
        fun () ->
          run_profile_stage
            ~stage_name:"Portfolio legacy flash"
            ~engine:Legacy_compat
            ~fraction
            ()
      in
      let modern_stage name selection fraction =
        fun () ->
          run_profile_stage
            ~stage_name:name
            ~engine:Modern_deep
            ~fraction
            ~env:[ "VIP_PASSIVE_SELECTION", Some selection ]
            ()
      in
      let fallback_stable () = fun () -> run_legacy_then_modern () in
      let stages =
        if term_heavy then
          [
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_TERM_CLASSIC_FRACTION" 0.55);
            modern_stage
              "Portfolio modern weight"
              "weight"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_TERM_WEIGHT_FRACTION" 0.25);
            fallback_stable ();
          ]
        else if clause_count < size_threshold && syn_shaped then
          [
            modern_stage
              "Portfolio modern SYN selection"
              "syn"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_SYN_FRACTION" 0.65);
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_CLASSIC_FRACTION" 0.20);
            fallback_stable ();
          ]
        else if unit_heavy || negative_heavy then
          [
            modern_stage
              "Portfolio modern short"
              "short"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_UNIT_SHORT_FRACTION" 0.55);
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_UNIT_CLASSIC_FRACTION" 0.25);
            fallback_stable ();
          ]
        else if clause_count < size_threshold then
          [
            legacy_stage (getenv_float "VIP_PORTFOLIO_SCHEDULE_LEGACY_FRACTION" 0.20);
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_CLASSIC_FRACTION" 0.50);
            modern_stage
              "Portfolio modern short"
              "short"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_SHORT_FRACTION" 0.15);
            fallback_stable ();
          ]
        else
          [
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_LARGE_CLASSIC_FRACTION" 0.45);
            modern_stage
              "Portfolio modern SYN selection"
              "syn"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_LARGE_SYN_FRACTION" 0.20);
            modern_stage
              "Portfolio modern short"
              "short"
              (getenv_float "VIP_PORTFOLIO_SCHEDULE_SHORT_FRACTION" 0.15);
            legacy_stage (getenv_float "VIP_PORTFOLIO_SCHEDULE_LARGE_LEGACY_FRACTION" 0.05);
            fallback_stable ();
          ]
      in
      if config.print_derivation then
        Printf.printf
          "%% Portfolio profile: clauses=%d literals=%d avg_clause_len=%.2f avg_lit_term=%.2f unit_ratio=%.2f negative_ratio=%.2f equality=%b legacy_sensitive=%b modern_biased=%b syn_shaped=%b\n%!"
          clause_count
          total_literals
          avg_clause_len
          avg_literal_term_size
          unit_ratio
          negative_ratio
          equality_problem
          legacy_sensitive_small
          modern_biased_small
          syn_shaped;
      if clause_count < size_threshold && (legacy_sensitive_small || modern_biased_small) then
        run_legacy_then_modern ()
      else
        run_schedule stages
    in

    let run_feq_modern () =
      let force_feq_schedule = getenv_bool "VIP_FEQ_MODERN_ALL" false in
      let modern_general_stage name selection fraction =
        fun () ->
          run_profile_stage
            ~stage_name:name
            ~engine:Modern_deep
            ~fraction
            ~env:[ "VIP_PASSIVE_SELECTION", Some selection ]
            ()
      in
      let legacy_general_stage name fraction =
        fun () ->
          run_profile_stage
            ~stage_name:name
            ~engine:Legacy_compat
            ~fraction
            ()
      in
      let legacy_remaining_stage name =
        fun () ->
          run_remaining_stage
            ~stage_name:name
            ~engine:Legacy_compat
            ()
      in
      let run_general_modern () =
        match problem_profile with
        | Large_general ->
            run_schedule
              [
                modern_general_stage
                  "General large modern classic"
                  "classic"
                  (getenv_float "VIP_GENERAL_LARGE_CLASSIC_FRACTION" 0.50);
                modern_general_stage
                  "General large modern short"
                  "short"
                  (getenv_float "VIP_GENERAL_LARGE_SHORT_FRACTION" 0.20);
                modern_general_stage
                  "General large modern weight"
                  "weight"
                  (getenv_float "VIP_GENERAL_LARGE_WEIGHT_FRACTION" 0.10);
                legacy_remaining_stage "General large legacy fallback";
              ]
        | Non_equality ->
            run_schedule
              [
                legacy_general_stage
                  "General legacy probe"
                  (getenv_float "VIP_GENERAL_LEGACY_FLASH_FRACTION" 0.25);
                modern_general_stage
                  "General modern classic"
                  "classic"
                  (getenv_float "VIP_GENERAL_CLASSIC_FRACTION" 0.35);
                modern_general_stage
                  "General modern short"
                  "short"
                  (getenv_float "VIP_GENERAL_SHORT_FRACTION" 0.20);
                modern_general_stage
                  "General modern weight"
                  "weight"
                  (getenv_float "VIP_GENERAL_WEIGHT_FRACTION" 0.10);
                legacy_remaining_stage "General legacy fallback";
              ]
        | Equality_light ->
            run_schedule
              [
                legacy_general_stage
                  "Equality-light legacy flash"
                  (getenv_float "VIP_EQUALITY_LIGHT_LEGACY_FLASH_FRACTION" 0.05);
                modern_general_stage
                  "Equality-light modern classic"
                  "classic"
                  (getenv_float "VIP_EQUALITY_LIGHT_CLASSIC_FRACTION" 0.65);
                modern_general_stage
                  "Equality-light modern weight"
                  "weight"
                  (getenv_float "VIP_EQUALITY_LIGHT_WEIGHT_FRACTION" 0.15);
                legacy_remaining_stage "Equality-light legacy fallback";
              ]
        | Equality_heavy ->
            run_legacy_then_modern ()
      in
      if (not force_feq_schedule) && problem_profile <> Equality_heavy then
        run_general_modern ()
      else
        let aw_ratio =
          match env_opt "VIP_FEQ_PASSIVE_AW_RATIO" with
          | Some s when String.trim s <> "" -> s
          | _ -> "1:4"
        in
        let feq_stage name selection fraction =
          fun () ->
            run_profile_stage
              ~stage_name:name
              ~engine:Modern_feq
              ~fraction
              ~env:
                [
                  ("VIP_PASSIVE_SELECTION", Some selection);
                  ("VIP_PASSIVE_AW_RATIO", Some aw_ratio);
                  ("VIP_FORWARD_SUBSUMPTION_RESOLUTION", Some "1");
                ]
              ()
        in
        run_schedule
          [
            (fun () ->
              let default_legacy_flash =
                if getenv_bool "VIP_DEFINITIONAL_CNF" false
                   || getenv_bool "VIP_AUTO_DEFINITIONAL_CNF" false then
                  0.0
                else
                  0.05
              in
              run_profile_stage
                ~stage_name:"FEQ legacy flash"
                ~engine:Legacy_compat
                ~fraction:
                  (getenv_float
                     "VIP_FEQ_LEGACY_FLASH_FRACTION"
                     default_legacy_flash)
                ());
            (fun () ->
              run_profile_stage
                ~stage_name:"FEQ stable modern"
                ~engine:Modern_deep
                ~fraction:(getenv_float "VIP_FEQ_STABLE_FRACTION" 0.35)
                ());
            feq_stage
              "FEQ unrestricted classic"
              "classic"
              (getenv_float "VIP_FEQ_CLASSIC_FRACTION" 0.30);
            feq_stage
              "FEQ unrestricted weight"
              "weight"
              (getenv_float "VIP_FEQ_WEIGHT_FRACTION" 0.15);
            feq_stage
              "FEQ unrestricted equality"
              "equality"
              (getenv_float "VIP_FEQ_EQUALITY_FRACTION" 0.10);
          ]
    in

    let run_modern_then_legacy () =
      let modern_fraction = getenv_float "VIP_PORTFOLIO_MODERN_FIRST_FRACTION" 0.80 in
      let modern_budget = fraction_budget modern_fraction in
      let modern_res =
        run_stage
          {
            stage_name = "Modern deep search";
            engine = Modern_deep;
            time_limit_s = modern_budget;
          }
      in
      match modern_res.stop_reason with
      | Refutation_found _ -> modern_res
      | Saturation | Time_limit | Clause_limit ->
          let remaining_time = remaining_time () in
          if remaining_time <= 0.1 then modern_res
          else
            run_stage
              {
                stage_name = "Legacy compatibility fallback";
                engine = Legacy_compat;
                time_limit_s = remaining_time;
              }
    in

    let ground_sat_result () =
      let empty = {
        Resolution.id = -1;
        parents = [];
        rule = "ground_sat_prefilter";
        clause_d = [];
        is_active = true;
      } in
      {
        Resolution.stop_reason = Refutation_found empty;
        derivation = [ empty ];
        stats = empty_resolution_stats (elapsed ());
      }
    in

    let ground_sat_prefilter_result =
      let enabled = getenv_bool "VIP_GROUND_SAT_PREFILTER" false in
      let max_clauses = getenv_int_global "VIP_GROUND_SAT_MAX_CLAUSES" 5000 in
      if enabled && clause_count <= max_clauses then
        let clauses = axioms @ support in
        let max_ground_instances =
          getenv_int_global "VIP_GROUND_SAT_MAX_INSTANCES" 300000
        in
        match ground_sat_unsat clauses with
        | Some true -> Some (ground_sat_result ())
        | Some false -> None
        | None ->
            begin
              match epr_ground_sat_unsat ~max_instances:max_ground_instances clauses with
              | Some true -> Some (ground_sat_result ())
              | Some false | None -> None
            end
      else
        None
    in

    let res =
      match ground_sat_prefilter_result with
      | Some res -> res
      | None ->
      match config.portfolio_mode with
      | Modern_then_legacy ->
          run_modern_then_legacy ()
      | Feq_modern ->
          run_feq_modern ()
      | Experimental_casc ->
          run_experimental_casc ()
      | Casc_aggressive ->
          run_casc_aggressive ()
      | Casc_240 ->
          run_casc_240 ()
      | Casc_150 ->
          run_casc_240 ~extended:true ()
      | Casc_feq_probe ->
          run_casc_feq_probe ()
      | Legacy_only ->
          run_stage
            {
              stage_name = "Legacy compatibility only";
              engine = Legacy_compat;
              time_limit_s = stage_time total_timeout;
            }
      | Modern_only ->
          run_stage
            {
              stage_name = "Modern deep only";
              engine = Modern_deep;
              time_limit_s = stage_time total_timeout;
            }
      | Modern_compat_only ->
          run_stage
            {
              stage_name = "Modern compat flash only";
              engine = Modern_compat_flash;
              time_limit_s = stage_time total_timeout;
            }
      | Scheduled_portfolio ->
          if getenv_bool "VIP_SCHEDULED_LEGACY_SCHEDULER" false then
            run_scheduled_portfolio ()
          else
            run_experimental_casc ()
      | Legacy_then_modern ->
          run_legacy_then_modern ()
    in

    let empty_clause =
      match res.stop_reason with
      | Refutation_found d -> Some d
      | Saturation | Time_limit | Clause_limit -> None
    in
    let proof_derivation =
      match empty_clause with
      | Some root -> proof_slice_to_root root res.derivation
      | None -> res.derivation
    in

    {
      status = infer_status_from_stop_reason ~has_conjecture res.stop_reason;
      info = {
        file = Some filename;
        clause_count;
        generated_clause_count = res.stats.generated_clauses;
        profile = string_of_problem_profile problem_profile;
        raw_clause_count;
        equality_literals;
        equality_literal_ratio;
        avg_literal_term_size;
        unit_ratio;
        negative_ratio;
        axiom_selection_enabled;
      };
      derivation = proof_derivation;
      empty_clause;
      resolution_stats = Some res.stats;
      tstp_prelude =
        (match empty_clause with
         | None -> None
         | Some _ ->
             (match !winning_tstp_prelude with
              | Some prelude -> Some prelude
              | None ->
                  Some
                    (build_tstp_prelude
                       ~problem:filename
                       parsed.inputs
                       traced_part.trace
                       proof_derivation)));
    }
  with
  | Clausify.Timeout_hit
  | Resolution.Timeout_hit
  | Stack_overflow ->
      ignore (Unix.alarm 0);
      timeout_outcome (elapsed ())
  | Sat_probe_success outcome ->
      outcome

let print_szs ?(verbose = true) outcome =
  let name =
    match outcome.info.file with
    | Some f -> f
    | None -> "<stdin>"
  in

  Printf.printf
    "%% SZS status %s for %s\n"
    (string_of_szs_status outcome.status)
    name;
  if not verbose then
    ()
  else begin
  Printf.printf "%% profile                 : %s\n" outcome.info.profile;
  Printf.printf "%% raw clauses             : %d\n" outcome.info.raw_clause_count;
  Printf.printf "%% equality literals       : %d\n" outcome.info.equality_literals;
  Printf.printf "%% equality literal ratio  : %.6f\n" outcome.info.equality_literal_ratio;
  Printf.printf "%% avg literal term size   : %.6f\n" outcome.info.avg_literal_term_size;
  Printf.printf "%% unit ratio              : %.6f\n" outcome.info.unit_ratio;
  Printf.printf "%% negative ratio          : %.6f\n" outcome.info.negative_ratio;
  Printf.printf "%% axiom selection         : %b\n" outcome.info.axiom_selection_enabled;

  match outcome.resolution_stats with
  | None -> ()
  | Some s ->
      Printf.printf "%% clauses input           : %d\n" outcome.info.clause_count;
      Printf.printf "%% clauses generated       : %d\n" s.generated_clauses;
      Printf.printf "%% clauses processed       : %d\n" s.processed_clauses;
      Printf.printf "%% resolution inferences   : %d\n" s.resolution_inferences;
      Printf.printf "%% factoring inferences    : %d\n" s.factoring_inferences;
      Printf.printf "%% equality resolution     : %d\n" s.equality_resolution_inferences;
      Printf.printf "%% equality factoring      : %d\n" s.equality_factoring_inferences;
      Printf.printf "%% superposition infer.    : %d\n" s.superposition_inferences;
      Printf.printf "%% demodulation rewrites   : %d\n" s.demodulation_rewrites;
      Printf.printf "%% subsumption tests       : %d\n" s.subsumption_tests;
      Printf.printf "%% subsumption rejections  : %d\n" s.subsumption_rejections;
      if s.avatar_enabled || s.avatar_successful_splits > 0 then begin
        Printf.printf "%% avatar enabled          : %b\n" s.avatar_enabled;
        Printf.printf "%% avatar keep original    : %b\n" s.avatar_keep_original;
        Printf.printf "%% avatar min split lits   : %d\n" s.avatar_min_split_literals;
        Printf.printf "%% avatar successful splits: %d\n" s.avatar_successful_splits;
        Printf.printf "%% avatar split vars       : %d/%d\n" s.avatar_split_vars_used s.avatar_max_split_vars;
        Printf.printf "%% avatar split attempts   : %d\n" s.avatar_split_attempts;
        Printf.printf "%% avatar split components : %d\n" s.avatar_split_components;
        Printf.printf "%% avatar reject short     : %d\n" s.avatar_split_rejected_short;
        Printf.printf "%% avatar reject equality  : %d\n" s.avatar_split_rejected_equality;
        Printf.printf "%% avatar reject nonground : %d\n" s.avatar_split_rejected_nonground;
        Printf.printf "%% avatar reject trivial   : %d\n" s.avatar_split_rejected_trivial;
        Printf.printf "%% avatar reject quota     : %d\n" s.avatar_split_rejected_quota;
        Printf.printf "%% avatar contextual empty : %d\n" s.avatar_contextual_empty_conflicts;
        Printf.printf "%% avatar sat clauses      : %d\n" s.avatar_sat_clauses_added;
        Printf.printf "%% avatar sat solves       : %d\n" s.avatar_sat_solves;
        Printf.printf "%% avatar context failures : %d\n" s.avatar_context_sat_failures;
        Printf.printf "%% avatar filtered infer.  : %d\n" s.avatar_filtered_inferences;
        Printf.printf "%% avatar sat conflicts    : %d\n" s.avatar_sat_conflicts
      end;
      Printf.printf "%% wall clock seconds      : %.6f\n" s.wall_clock_s
  end
