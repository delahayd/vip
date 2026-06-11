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

type engine_kind =
  | Legacy_compat
  | Modern_compat_flash
  | Modern_deep

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

let run_file ?(config = default_config) filename =
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

    let axioms, support =
      if config.use_sos then (part.axioms, part.support)
      else ([], part.axioms @ part.support)
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

    let literal_contains_equality = function
      | Types.Pos { pred = "="; args = [ _; _ ] }
      | Types.Neg { pred = "="; args = [ _; _ ] } -> true
      | _ -> false
    in
    let clause_contains_equality c = List.exists literal_contains_equality c in
    let equality_problem =
      List.exists clause_contains_equality axioms
      || List.exists clause_contains_equality support
    in

    let compat_mode () =
      if equality_problem then Resolution.Unrestricted else config.inference_mode
    in

    let run_modern_resolution ~time_limit_s ~expensive_simplifications ~emulate_v1 =
      let limits = {
        Resolution.time_limit_s = Some time_limit_s;
        max_generated_clauses = config.max_generated_clauses;
      } in
      let mode = if emulate_v1 then compat_mode () else config.inference_mode in
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
    in

    let run_modern_deep ~time_limit_s =
      run_modern_resolution
        ~time_limit_s
        ~expensive_simplifications:true
        ~emulate_v1:false
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
      let size_threshold = getenv_int "IP_PORTFOLIO_SIZE_THRESHOLD" 85 in
      let default_flash_fraction, default_modern_fraction =
        if clause_count < size_threshold then
          (getenv_float "IP_PORTFOLIO_SMALL_LEGACY_FRACTION" 0.50,
           getenv_float "IP_PORTFOLIO_SMALL_MODERN_FRACTION" 0.50)
        else
          (getenv_float "IP_PORTFOLIO_LARGE_LEGACY_FLASH_FRACTION" 0.0,
           getenv_float "IP_PORTFOLIO_LARGE_MODERN_FRACTION" 0.70)
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
