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
}

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

let clauses_of_file filename =
  let parsed = load_problem filename in
  let clauses, _report = clauses_of_input_with_report parsed.inputs in
  clauses

let infer_status_from_stop_reason = function
  | Refutation_found _ -> Unsatisfiable
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


let getenv_bool name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some s ->
      begin
        match String.lowercase_ascii (String.trim s) with
        | "1" | "true" | "yes" | "on" -> true
        | "0" | "false" | "no" | "off" -> false
        | _ -> default
      end

let getenv_int_global name default =
  match Sys.getenv_opt name with
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
  let debug = getenv_bool "IP_GROUND_SAT_DEBUG" false in
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
  let debug = getenv_bool "IP_GROUND_SAT_DEBUG" false in
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
    let min_axioms = getenv_int_global "IP_AXIOM_SELECTION_MIN_AXIOMS" 100 in
    if List.length axioms < min_axioms then
      axioms
    else
      let default_rounds = if support = [] then 1 else 6 in
      let max_rounds = getenv_int_global "IP_AXIOM_SELECTION_ROUNDS" default_rounds in
      let max_axioms =
        match Sys.getenv_opt "IP_AXIOM_SELECTION_MAX_AXIOMS" with
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
        getenv_int_global "IP_AXIOM_SELECTION_MAX_SYMBOL_FREQ" 128
      in
      let selectable_symbols syms =
        Types.StringSet.filter
          (fun s ->
            let freq = symbol_frequency s in
            (freq = 0 || freq <= max_symbol_freq) && not (String.equal s "p:="))
          syms
      in
      let rare_seed_symbols () =
        let max_freq = getenv_int_global "IP_AXIOM_SELECTION_RARE_MAX_FREQ" 2 in
        let max_seeds = getenv_int_global "IP_AXIOM_SELECTION_RARE_MAX_SYMBOLS" 64 in
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
      let initial_symbols =
        if support = [] then rare_seed_symbols ()
        else selectable_symbols (clauses_symbols support)
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
      let rec rounds n =
        check_timeout ();
        if n <= 0 then ()
        else
          let changed = ref false in
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
        if getenv_bool "IP_AXIOM_SELECTION_DEBUG" false then
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
    }
  in

  try
    let load_config =
      match config.tptp_dir with
      | None -> default_load_config
      | Some d -> { include_paths = [ d ]; use_tptp_env = true }
    in
    let parsed = load_problem ~config:load_config filename in
    check_problem_timeout ();
    let part = partition_input_clauses ~check_timeout:check_problem_timeout parsed.inputs in

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
      raw_equality_literals >= getenv_int_global "IP_PROFILE_EQ_HEAVY_MIN_LITERALS" 4
      && raw_equality_literal_ratio
         >= (match Sys.getenv_opt "IP_PROFILE_EQ_HEAVY_MIN_RATIO" with
             | Some s -> (try float_of_string s with Failure _ -> 0.12)
             | None -> 0.12)
      && raw_avg_literal_term_size
         >= (match Sys.getenv_opt "IP_PROFILE_EQ_HEAVY_MIN_AVG_TERM" with
             | Some s -> (try float_of_string s with Failure _ -> 3.5)
             | None -> 3.5)
    in
    let problem_profile =
      if equality_heavy then Equality_heavy
      else if raw_equality_problem then Equality_light
      else if raw_clause_count >= getenv_int_global "IP_PROFILE_LARGE_MIN_CLAUSES" 500 then Large_general
      else Non_equality
    in
    let axiom_selection_enabled =
      getenv_bool "IP_AXIOM_SELECTION" false
      && (problem_profile = Equality_heavy || getenv_bool "IP_AXIOM_SELECTION_ALL" false)
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

    let timeout_result wall_clock_s =
      {
        Resolution.stop_reason = Time_limit;
        derivation = [];
        stats = {
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
        };
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
    if getenv_bool "IP_PROFILE_DEBUG" false then
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
      match Sys.getenv_opt name with
      | None -> default
      | Some s ->
          (try max 0.0 (float_of_string s) with Failure _ -> default)
    in

    let getenv_int name default =
      match Sys.getenv_opt name with
      | None -> default
      | Some s ->
          (try max 0 (int_of_string s) with Failure _ -> default)
    in

    let getenv_float_opt name =
      match Sys.getenv_opt name with
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
      let size_threshold = getenv_int "IP_PORTFOLIO_SIZE_THRESHOLD" 160 in
      let modern_biased_small =
        clause_count <= getenv_int "IP_PORTFOLIO_TINY_MODERN_MAX_CLAUSES" 4
        || ((clause_count >= getenv_int "IP_PORTFOLIO_MEDIUM_MODERN_MIN_CLAUSES" 13
             && clause_count <= getenv_int "IP_PORTFOLIO_MEDIUM_MODERN_MAX_CLAUSES" 24)
            && not (clause_count >= getenv_int "IP_PORTFOLIO_MEDIUM_LEGACY_MIN_CLAUSES" 18
                    && clause_count <= getenv_int "IP_PORTFOLIO_MEDIUM_LEGACY_MAX_CLAUSES" 20))
      in
      let modern_only_threshold = getenv_int "IP_PORTFOLIO_MODERN_ONLY_MIN_CLAUSES" 80 in
      let default_flash_fraction, default_modern_fraction =
        if clause_count >= modern_only_threshold then
          (getenv_float "IP_PORTFOLIO_LARGE_LEGACY_FLASH_FRACTION" 0.0,
           getenv_float "IP_PORTFOLIO_LARGE_MODERN_FRACTION" 1.0)
        else if clause_count < size_threshold && modern_biased_small then
          (getenv_float "IP_PORTFOLIO_SMALL_LEGACY_FRACTION" 0.05,
           getenv_float "IP_PORTFOLIO_SMALL_MODERN_FRACTION" 0.95)
        else if clause_count < size_threshold then
          (getenv_float "IP_PORTFOLIO_SMALL_LEGACY_FRACTION" 0.50,
           getenv_float "IP_PORTFOLIO_SMALL_MODERN_FRACTION" 0.50)
        else
          (getenv_float "IP_PORTFOLIO_LARGE_LEGACY_FLASH_FRACTION" 0.0,
           getenv_float "IP_PORTFOLIO_LARGE_MODERN_FRACTION" 1.0)
      in
      let flash_fraction =
        match getenv_float_opt "IP_PORTFOLIO_LEGACY_FLASH_FRACTION" with
        | Some f -> f
        | None -> default_flash_fraction
      in
      let modern_fraction =
        match getenv_float_opt "IP_PORTFOLIO_MODERN_FRACTION" with
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
      let old = Sys.getenv_opt name in
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

    let run_schedule stages =
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

    let run_experimental_subrun ~stage_name ~portfolio_mode ~fraction ?(env = []) () =
      let budget = fraction_budget fraction in
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
      let small_non_equality =
        problem_profile = Non_equality
        && raw_clause_count
           <= getenv_int "IP_EXPERIMENTAL_SMALL_NON_EQ_MAX_CLAUSES" 200
        && raw_equality_literals = 0
      in
      let compact_non_unit_non_equality =
        small_non_equality
        && raw_clause_count
           <= getenv_int "IP_EXPERIMENTAL_COMPACT_NON_UNIT_MAX_CLAUSES" 30
        && unit_ratio <= getenv_float "IP_EXPERIMENTAL_COMPACT_NON_UNIT_MAX_UNIT_RATIO" 0.05
      in
      let avatar_fallback () =
        run_remaining_stage
          ~stage_name:"Experimental AVATAR fallback"
          ~engine:Modern_deep
          ~env:
            [
              "IP_AVATAR_SPLITTING", Some "1";
              "IP_AVATAR_GROUND_ONLY", Some "0";
              "IP_AVATAR_KEEP_ORIGINAL", Some "1";
              "IP_AVATAR_MIN_SPLIT", Some "4";
              "IP_AVATAR_MAX_SPLIT_VARS", Some "8";
              "IP_AVATAR_MAX_SPLIT_VARS_PER_CLAUSE", Some "3";
              "IP_AVATAR_MODEL_FALSE_FIRST", Some "1";
            ]
          ()
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
                   "IP_EXPERIMENTAL_COMPACT_NON_UNIT_MODERN_FRACTION"
                   0.85)
              ~env:[ "IP_PASSIVE_SELECTION", Some "classic" ];
            run_experimental_subrun
              ~stage_name:"Experimental compact non-unit legacy-guided"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float
                   "IP_EXPERIMENTAL_COMPACT_NON_UNIT_LEGACY_GUIDED_FRACTION"
                   0.10)
              ~env:
                [
                  "IP_PASSIVE_SELECTION", Some "legacy";
                  "IP_LITERAL_SELECTION", Some "legacy";
                  "IP_RESOLUTION_LITERAL_SELECTION", Some "legacy";
                  "IP_LEGACY_SUBSUMPTION", Some "1";
                  "IP_FAST_CONDENSATION", Some "0";
                  "IP_FORWARD_SUBSUMPTION_RESOLUTION", Some "0";
                ];
            run_remaining_stage
              ~stage_name:"Experimental compact non-unit legacy fallback"
              ~engine:Legacy_compat;
          ]
      else if small_non_equality then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality modern portfolio"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float
                   "IP_EXPERIMENTAL_SMALL_MODERN_PORTFOLIO_FRACTION"
                   0.60);
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality modern legacy-guided"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float
                   "IP_EXPERIMENTAL_SMALL_LEGACY_GUIDED_FRACTION"
                   0.15)
              ~env:
                [
                  "IP_PASSIVE_SELECTION", Some "legacy";
                  "IP_LITERAL_SELECTION", Some "legacy";
                  "IP_RESOLUTION_LITERAL_SELECTION", Some "legacy";
                  "IP_LEGACY_SUBSUMPTION", Some "1";
                  "IP_FAST_CONDENSATION", Some "0";
                  "IP_FORWARD_SUBSUMPTION_RESOLUTION", Some "0";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality legacy fallback"
              ~portfolio_mode:Legacy_only
              ~fraction:
                (getenv_float "IP_EXPERIMENTAL_SMALL_LEGACY_FRACTION" 0.25);
            run_experimental_subrun
              ~stage_name:"Experimental small non-equality modern classic"
              ~portfolio_mode:Modern_only
              ~fraction:
                (getenv_float "IP_EXPERIMENTAL_SMALL_MODERN_FRACTION" 0.10)
              ~env:[ "IP_PASSIVE_SELECTION", Some "classic" ];
            run_remaining_stage
              ~stage_name:"Experimental small non-equality stable fallback"
              ~engine:Legacy_compat;
          ]
      else if large_general then
        run_schedule
          [
            run_experimental_subrun
              ~stage_name:"Experimental large unselected first"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "IP_EXPERIMENTAL_LARGE_FULL_FIRST_FRACTION" 1.0)
              ~env:
                [
                  "IP_AXIOM_SELECTION", Some "0";
                  "IP_AXIOM_SELECTION_ALL", Some "0";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental large SInE narrow"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "IP_EXPERIMENTAL_SINE_NARROW_FRACTION" 0.0)
              ~env:
                [
                  "IP_AXIOM_SELECTION", Some "1";
                  "IP_AXIOM_SELECTION_ALL", Some "1";
                  "IP_AXIOM_SELECTION_MAX_AXIOMS", Some "300";
                  "IP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "128";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental large SInE medium"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "IP_EXPERIMENTAL_SINE_MEDIUM_FRACTION" 0.0)
              ~env:
                [
                  "IP_AXIOM_SELECTION", Some "1";
                  "IP_AXIOM_SELECTION_ALL", Some "1";
                  "IP_AXIOM_SELECTION_MAX_AXIOMS", Some "1000";
                  "IP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "256";
                ];
            run_experimental_subrun
              ~stage_name:"Experimental large SInE wide"
              ~portfolio_mode:Feq_modern
              ~fraction:
                (getenv_float "IP_EXPERIMENTAL_SINE_WIDE_FRACTION" 0.0)
              ~env:
                [
                  "IP_AXIOM_SELECTION", Some "1";
                  "IP_AXIOM_SELECTION_ALL", Some "1";
                  "IP_AXIOM_SELECTION_MAX_AXIOMS", Some "2500";
                  "IP_AXIOM_SELECTION_MAX_SYMBOL_FREQ", Some "512";
                ];
          ]
      else if problem_profile = Equality_light then
        run_experimental_subrun
          ~stage_name:"Experimental equality-light FEQ full schedule"
          ~portfolio_mode:Feq_modern
          ~fraction:
            (getenv_float
               "IP_EXPERIMENTAL_EQUALITY_LIGHT_FEQ_ALL_FRACTION"
               0.70)
          ~env:[ "IP_FEQ_MODERN_ALL", Some "1" ]
          ()
        |> fun first ->
        begin
          match first.stop_reason with
          | Refutation_found _ -> first
          | Saturation | Time_limit | Clause_limit -> avatar_fallback ()
        end
      else
        run_experimental_subrun
          ~stage_name:"Experimental stable fallback"
          ~portfolio_mode:Feq_modern
          ~fraction:(getenv_float "IP_EXPERIMENTAL_STABLE_FRACTION" 0.70)
          ()
        |> fun first ->
        match first.stop_reason with
        | Refutation_found _ -> first
        | Saturation | Time_limit | Clause_limit ->
            avatar_fallback ()
    in

    let run_scheduled_portfolio () =
      let size_threshold = getenv_int "IP_PORTFOLIO_SIZE_THRESHOLD" 160 in
      let modern_biased_small =
        clause_count <= getenv_int "IP_PORTFOLIO_TINY_MODERN_MAX_CLAUSES" 4
        || ((clause_count >= getenv_int "IP_PORTFOLIO_MEDIUM_MODERN_MIN_CLAUSES" 13
             && clause_count <= getenv_int "IP_PORTFOLIO_MEDIUM_MODERN_MAX_CLAUSES" 24)
            && not (clause_count >= getenv_int "IP_PORTFOLIO_MEDIUM_LEGACY_MIN_CLAUSES" 18
                    && clause_count <= getenv_int "IP_PORTFOLIO_MEDIUM_LEGACY_MAX_CLAUSES" 20))
      in
      let syn_min = getenv_int "IP_PASSIVE_SYN_MIN_CLAUSES" 40 in
      let syn_max = getenv_int "IP_PASSIVE_SYN_MAX_CLAUSES" 55 in
      let syn_shaped = clause_count >= syn_min && clause_count <= syn_max in
      let legacy_sensitive_small =
        clause_count > getenv_int "IP_PORTFOLIO_TINY_MODERN_MAX_CLAUSES" 4
        && clause_count < getenv_int "IP_PORTFOLIO_LEGACY_ONLY_SMALL_MAX_CLAUSES" 13
      in
      let unit_heavy =
        unit_ratio >= getenv_float "IP_PORTFOLIO_UNIT_HEAVY_RATIO" 0.55
      in
      let negative_heavy =
        negative_ratio >= getenv_float "IP_PORTFOLIO_NEGATIVE_HEAVY_RATIO" 0.65
      in
      let term_heavy =
        equality_problem
        || avg_literal_term_size >= getenv_float "IP_PORTFOLIO_TERM_HEAVY_AVG" 6.0
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
            ~env:[ "IP_PASSIVE_SELECTION", Some selection ]
            ()
      in
      let fallback_stable () = fun () -> run_legacy_then_modern () in
      let stages =
        if term_heavy then
          [
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_TERM_CLASSIC_FRACTION" 0.55);
            modern_stage
              "Portfolio modern weight"
              "weight"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_TERM_WEIGHT_FRACTION" 0.25);
            fallback_stable ();
          ]
        else if clause_count < size_threshold && syn_shaped then
          [
            modern_stage
              "Portfolio modern SYN selection"
              "syn"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_SYN_FRACTION" 0.65);
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_CLASSIC_FRACTION" 0.20);
            fallback_stable ();
          ]
        else if unit_heavy || negative_heavy then
          [
            modern_stage
              "Portfolio modern short"
              "short"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_UNIT_SHORT_FRACTION" 0.55);
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_UNIT_CLASSIC_FRACTION" 0.25);
            fallback_stable ();
          ]
        else if clause_count < size_threshold then
          [
            legacy_stage (getenv_float "IP_PORTFOLIO_SCHEDULE_LEGACY_FRACTION" 0.20);
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_CLASSIC_FRACTION" 0.50);
            modern_stage
              "Portfolio modern short"
              "short"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_SHORT_FRACTION" 0.15);
            fallback_stable ();
          ]
        else
          [
            modern_stage
              "Portfolio modern classic"
              "classic"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_LARGE_CLASSIC_FRACTION" 0.45);
            modern_stage
              "Portfolio modern SYN selection"
              "syn"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_LARGE_SYN_FRACTION" 0.20);
            modern_stage
              "Portfolio modern short"
              "short"
              (getenv_float "IP_PORTFOLIO_SCHEDULE_SHORT_FRACTION" 0.15);
            legacy_stage (getenv_float "IP_PORTFOLIO_SCHEDULE_LARGE_LEGACY_FRACTION" 0.05);
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
      let force_feq_schedule = getenv_bool "IP_FEQ_MODERN_ALL" false in
      let modern_general_stage name selection fraction =
        fun () ->
          run_profile_stage
            ~stage_name:name
            ~engine:Modern_deep
            ~fraction
            ~env:[ "IP_PASSIVE_SELECTION", Some selection ]
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
                  (getenv_float "IP_GENERAL_LARGE_CLASSIC_FRACTION" 0.50);
                modern_general_stage
                  "General large modern short"
                  "short"
                  (getenv_float "IP_GENERAL_LARGE_SHORT_FRACTION" 0.20);
                modern_general_stage
                  "General large modern weight"
                  "weight"
                  (getenv_float "IP_GENERAL_LARGE_WEIGHT_FRACTION" 0.10);
                legacy_remaining_stage "General large legacy fallback";
              ]
        | Non_equality ->
            run_schedule
              [
                legacy_general_stage
                  "General legacy probe"
                  (getenv_float "IP_GENERAL_LEGACY_FLASH_FRACTION" 0.25);
                modern_general_stage
                  "General modern classic"
                  "classic"
                  (getenv_float "IP_GENERAL_CLASSIC_FRACTION" 0.35);
                modern_general_stage
                  "General modern short"
                  "short"
                  (getenv_float "IP_GENERAL_SHORT_FRACTION" 0.20);
                modern_general_stage
                  "General modern weight"
                  "weight"
                  (getenv_float "IP_GENERAL_WEIGHT_FRACTION" 0.10);
                legacy_remaining_stage "General legacy fallback";
              ]
        | Equality_light ->
            run_schedule
              [
                legacy_general_stage
                  "Equality-light legacy flash"
                  (getenv_float "IP_EQUALITY_LIGHT_LEGACY_FLASH_FRACTION" 0.05);
                modern_general_stage
                  "Equality-light modern classic"
                  "classic"
                  (getenv_float "IP_EQUALITY_LIGHT_CLASSIC_FRACTION" 0.65);
                modern_general_stage
                  "Equality-light modern weight"
                  "weight"
                  (getenv_float "IP_EQUALITY_LIGHT_WEIGHT_FRACTION" 0.15);
                legacy_remaining_stage "Equality-light legacy fallback";
              ]
        | Equality_heavy ->
            run_legacy_then_modern ()
      in
      if (not force_feq_schedule) && problem_profile <> Equality_heavy then
        run_general_modern ()
      else
        let aw_ratio =
          match Sys.getenv_opt "IP_FEQ_PASSIVE_AW_RATIO" with
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
                  ("IP_PASSIVE_SELECTION", Some selection);
                  ("IP_PASSIVE_AW_RATIO", Some aw_ratio);
                  ("IP_FORWARD_SUBSUMPTION_RESOLUTION", Some "1");
                ]
              ()
        in
        run_schedule
          [
            (fun () ->
              run_profile_stage
                ~stage_name:"FEQ legacy flash"
                ~engine:Legacy_compat
                ~fraction:(getenv_float "IP_FEQ_LEGACY_FLASH_FRACTION" 0.05)
                ());
            (fun () ->
              run_profile_stage
                ~stage_name:"FEQ stable modern"
                ~engine:Modern_deep
                ~fraction:(getenv_float "IP_FEQ_STABLE_FRACTION" 0.35)
                ());
            feq_stage
              "FEQ unrestricted classic"
              "classic"
              (getenv_float "IP_FEQ_CLASSIC_FRACTION" 0.30);
            feq_stage
              "FEQ unrestricted weight"
              "weight"
              (getenv_float "IP_FEQ_WEIGHT_FRACTION" 0.15);
            feq_stage
              "FEQ unrestricted equality"
              "equality"
              (getenv_float "IP_FEQ_EQUALITY_FRACTION" 0.10);
          ]
    in

    let run_modern_then_legacy () =
      let modern_fraction = getenv_float "IP_PORTFOLIO_MODERN_FIRST_FRACTION" 0.80 in
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
      let enabled = getenv_bool "IP_GROUND_SAT_PREFILTER" false in
      let max_clauses = getenv_int_global "IP_GROUND_SAT_MAX_CLAUSES" 5000 in
      if enabled && clause_count <= max_clauses then
        let clauses = axioms @ support in
        let max_ground_instances =
          getenv_int_global "IP_GROUND_SAT_MAX_INSTANCES" 300000
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
          if getenv_bool "IP_SCHEDULED_LEGACY_SCHEDULER" false then
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

    {
      status = infer_status_from_stop_reason res.stop_reason;
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
      derivation = res.derivation;
      empty_clause;
      resolution_stats = Some res.stats;
    }
  with
  | Clausify.Timeout_hit
  | Resolution.Timeout_hit ->
      ignore (Unix.alarm 0);
      timeout_outcome (elapsed ())

let print_szs outcome =
  let name =
    match outcome.info.file with
    | Some f -> f
    | None -> "<stdin>"
  in

  Printf.printf
    "%% SZS status %s for %s\n"
    (string_of_szs_status outcome.status)
    name;
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
