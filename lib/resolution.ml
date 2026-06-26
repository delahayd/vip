open Types
open Subst
open Unif
open Clause
open Pretty

exception Timeout_hit
exception Rewrite_limit_hit

type inference_mode =
  | Unrestricted
  | Ordered
  | Ordered_with_fallback

type split_var = int

type prop_lit =
  | PPos of split_var
  | PNeg of split_var

type context = prop_lit list

type derived = {
  id : int;
  parents : int list;
  rule : string;
  clause_d : clause;
  mutable is_active : bool;
}

type passive_entry = {
  passive_id : int;
  age : int;
  weight : int;
  d : derived;
}

type limits = {
  time_limit_s : float option;
  max_generated_clauses : int option;
}

type stop_reason =
  | Refutation_found of derived
  | Saturation
  | Time_limit
  | Clause_limit

type stats = {
  generated_clauses : int;
  processed_clauses : int;
  resolution_inferences : int;
  factoring_inferences : int;
  equality_resolution_inferences : int;
  equality_factoring_inferences : int;
  superposition_inferences : int;
  demodulation_rewrites : int;
  subsumption_tests : int;
  subsumption_rejections : int;
  avatar_enabled : bool;
  avatar_keep_original : bool;
  avatar_min_split_literals : int;
  avatar_max_split_vars : int;
  avatar_split_vars_used : int;
  avatar_split_attempts : int;
  avatar_successful_splits : int;
  avatar_split_components : int;
  avatar_split_rejected_disabled : int;
  avatar_split_rejected_short : int;
  avatar_split_rejected_equality : int;
  avatar_split_rejected_nonground : int;
  avatar_split_rejected_trivial : int;
  avatar_split_rejected_quota : int;
  avatar_contextual_empty_conflicts : int;
  avatar_sat_clauses_added : int;
  avatar_sat_solves : int;
  avatar_sat_conflicts : int;
  avatar_context_sat_tests : int;
  avatar_context_sat_failures : int;
  avatar_filtered_inferences : int;
  wall_clock_s : float;
}

type run_result = {
  stop_reason : stop_reason;
  derivation : derived list;
  stats : stats;
}

let default_limits = {
  time_limit_s = None;
  max_generated_clauses = None;
}

let max_demodulation_steps_per_term = 10_000

let id_counter = ref 0

let reset_id_counter () =
  id_counter := 0

let next_id () =
  incr id_counter;
  !id_counter

let option_exists f = function
  | Some x -> f x
  | None -> false

let unique_ids ids =
  let seen = Hashtbl.create (List.length ids + 1) in
  List.filter
    (fun id ->
      if Hashtbl.mem seen id then false
      else begin
        Hashtbl.add seen id ();
        true
      end)
    ids

let string_of_prop_lit = function
  | PPos v -> "p" ^ string_of_int v
  | PNeg v -> "~p" ^ string_of_int v

let normalize_context ctx =
  ctx |> List.sort_uniq compare

let string_of_context ctx =
  match normalize_context ctx with
  | [] -> ""
  | ctx -> String.concat "&" (List.map string_of_prop_lit ctx)

let context_subsumes a b =
  let a = normalize_context a in
  let b = normalize_context b in
  List.for_all (fun lit -> List.exists (( = ) lit) b) a

let neg_prop_lit = function
  | PPos v -> PNeg v
  | PNeg v -> PPos v

module Prop_sat = struct
  type t = {
    mutable next_var : int;
    clauses : prop_lit list list ref;
  }

  let create () = { next_var = 0; clauses = ref [] }

  let new_var st =
    st.next_var <- st.next_var + 1;
    st.next_var

  let add_clause st clause =
    st.clauses := normalize_context clause :: !(st.clauses)

  let var_of_lit = function PPos v | PNeg v -> v
  let sign_of_lit = function PPos _ -> true | PNeg _ -> false

  let eval_lit assign lit =
    match Hashtbl.find_opt assign (var_of_lit lit) with
    | None -> None
    | Some v -> Some (v = sign_of_lit lit)

  let set_lit assign lit =
    let var = var_of_lit lit in
    let value = sign_of_lit lit in
    match Hashtbl.find_opt assign var with
    | None -> Hashtbl.add assign var value; true
    | Some old -> old = value

  let vars_of_clauses clauses =
    clauses
    |> List.concat
    |> List.map var_of_lit
    |> List.sort_uniq compare

  let rec propagate clauses assign =
    let changed = ref false in
    let conflict = ref false in
    List.iter
      (fun clause ->
        if not !conflict then begin
          let satisfied = ref false in
          let unassigned = ref [] in
          List.iter
            (fun lit ->
              match eval_lit assign lit with
              | Some true -> satisfied := true
              | Some false -> ()
              | None -> unassigned := lit :: !unassigned)
            clause;
          if not !satisfied then
            match !unassigned with
            | [] -> conflict := true
            | [ lit ] ->
                if set_lit assign lit then changed := true
                else conflict := true
            | _ -> ()
        end)
      clauses;
    if !conflict then false
    else if !changed then propagate clauses assign
    else true

  let copy_assign assign =
    let copy = Hashtbl.create (Hashtbl.length assign) in
    Hashtbl.iter (fun k v -> Hashtbl.add copy k v) assign;
    copy

  let model ?(prefer_false = false) st assumptions =
    let clauses = List.map (fun lit -> [ lit ]) assumptions @ !(st.clauses) in
    let vars = vars_of_clauses clauses in
    let rec search assign =
      if not (propagate clauses assign) then None
      else
        match List.find_opt (fun v -> not (Hashtbl.mem assign v)) vars with
        | None -> Some assign
        | Some v ->
            let first_value, second_value =
              if prefer_false then (false, true) else (true, false)
            in
            let assign_first = copy_assign assign in
            Hashtbl.add assign_first v first_value;
            (match search assign_first with
             | Some _ as m -> m
             | None ->
                 let assign_second = copy_assign assign in
                 Hashtbl.add assign_second v second_value;
                 search assign_second)
    in
    search (Hashtbl.create 17)

  let satisfiable st assumptions =
    match model st assumptions with
    | Some _ -> true
    | None -> false

  let lit_true_in_model assign lit =
    match eval_lit assign lit with
    | Some true -> true
    | Some false | None -> false
end

let atom_of_literal = function
  | Pos a | Neg a -> a

let negate_literal = function
  | Pos a -> Neg a
  | Neg a -> Pos a

let sign = function
  | Pos _ -> true
  | Neg _ -> false

let is_negative = function
  | Neg _ -> true
  | Pos _ -> false

let positive_equality = function
  | Pos { pred = "="; args = [ l; r ] } -> Some (l, r)
  | _ -> None

let negative_equality = function
  | Neg { pred = "="; args = [ l; r ] } -> Some (l, r)
  | _ -> None

let literal_contains_equality = function
  | Pos { pred = "="; args = [ _; _ ] }
  | Neg { pred = "="; args = [ _; _ ] } -> true
  | _ -> false

let clause_contains_equality c =
  List.exists literal_contains_equality c

let all_indices c =
  let rec aux i acc = function
    | [] -> List.rev acc
    | _ :: tl -> aux (i + 1) (i :: acc) tl
  in
  aux 0 [] c

let negative_indices c =
  c
  |> List.mapi (fun i lit -> i, lit)
  |> List.filter (function _, Neg _ -> true | _ -> false)
  |> List.map fst

let selected_or_maximal_indices ~emulate_v1 c =
  let literal_selection =
    if emulate_v1 then "legacy"
    else
      match Sys.getenv_opt "IP_LITERAL_SELECTION" with
      | Some s when String.trim s <> "" -> String.lowercase_ascii (String.trim s)
      | Some _ | None -> "first-negative"
  in
  let choose_by_weight cmp indices =
    match indices with
    | [] -> []
    | i :: rest ->
        let best =
          List.fold_left
            (fun best i ->
              let wb = literal_weight (List.nth c best) in
              let wi = literal_weight (List.nth c i) in
              if cmp wi wb then i else best)
            i
            rest
        in
        [ best ]
  in
  let negs = negative_indices c in
  if negs <> [] then
    match literal_selection with
    | "legacy" | "all-negative" | "all-neg" -> negs
    | "smallest-negative" | "smallest-neg" | "small-neg" ->
        choose_by_weight ( < ) negs
    | "largest-negative" | "largest-neg" | "heavy-neg" ->
        choose_by_weight ( > ) negs
    | "ground-negative" | "ground-neg" ->
        begin
          match
            List.filter
              (fun i ->
                let lit = List.nth c i in
                vars_of_clause [ lit ] |> Types.StringSet.is_empty)
              negs
          with
          | [] -> [ List.hd negs ]
          | ground_negs -> choose_by_weight ( < ) ground_negs
        end
    | _ ->
        (* Efficient default: pick only the first negative literal. *)
        [ List.hd negs ]
  else if literal_selection = "legacy" then
    Ordering.maximal_literal_indices c
  else
    all_indices c

let is_active_literal ~emulate_v1 mode c i =
  match mode with
  | Unrestricted -> true
  | Ordered | Ordered_with_fallback ->
      List.exists (( = ) i) (selected_or_maximal_indices ~emulate_v1 c)

let dedup_clauses ?(check_timeout = fun () -> ()) cls =
  let seen = Hashtbl.create 1024 in
  let rec aux acc = function
    | [] -> List.rev acc
    | c :: rest ->
        check_timeout ();
        let key = string_of_clause c in
        if Hashtbl.mem seen key then aux acc rest
        else begin
          Hashtbl.add seen key ();
          aux (c :: acc) rest
        end
  in
  aux [] cls

let dedup_clauses_with_parents ?(check_timeout = fun () -> ()) items =
  let seen = Hashtbl.create 1024 in
  let rec aux acc = function
    | [] -> List.rev acc
    | (c, parents) :: rest ->
        check_timeout ();
        let key = string_of_clause c in
        if Hashtbl.mem seen key then aux acc rest
        else begin
          Hashtbl.add seen key ();
          aux ((c, parents) :: acc) rest
        end
  in
  aux [] items

type demodulator = term * term

type demod_index = {
  by_root : (string * int, demodulator list ref) Hashtbl.t;
  mutable variable_lhs : demodulator list;
  mutable all_rules : demodulator list;
}

let create_demod_index () =
  { by_root = Hashtbl.create 251; variable_lhs = []; all_rules = [] }

let root_key_of_term = function
  | Var _ -> None
  | Fun (f, args) -> Some (f, List.length args)

let add_demod_index idx ((lhs, _) as rule) =
  idx.all_rules <- rule :: idx.all_rules;
  match root_key_of_term lhs with
  | None -> idx.variable_lhs <- rule :: idx.variable_lhs
  | Some key ->
      let bucket =
        match Hashtbl.find_opt idx.by_root key with
        | Some r -> r
        | None ->
            let r = ref [] in
            Hashtbl.add idx.by_root key r;
            r
      in
      bucket := rule :: !bucket

let demod_candidates idx t =
  match root_key_of_term t with
  | None -> idx.variable_lhs
  | Some key ->
      let rooted =
        match Hashtbl.find_opt idx.by_root key with
        | None -> []
        | Some r -> !r
      in
      rooted @ idx.variable_lhs

let rewrite_fix_term_with_candidates ~check_timeout ~candidates count t =
  let count = ref count in
  let rec aux t =
    check_timeout ();
    if !count > max_demodulation_steps_per_term then
      raise Rewrite_limit_hit;
    match
      List.find_map
        (fun (l, r) ->
          try
            let s = Match.match_terms l t empty_subst in
            Some (apply_subst_term s r)
          with Match.Not_matchable -> None)
        (candidates t)
    with
    | Some t' ->
        incr count;
        aux t'
    | None ->
        match t with
        | Var _ -> t
        | Fun (f, args) -> Fun (f, List.map aux args)
  in
  let t' = aux t in
  t', !count

let rewrite_fix_term ~check_timeout demods count t =
  rewrite_fix_term_with_candidates
    ~check_timeout
    ~candidates:(fun _ -> demods)
    count
    t

let rewrite_fix_term_indexed ~check_timeout idx count t =
  rewrite_fix_term_with_candidates
    ~check_timeout
    ~candidates:(demod_candidates idx)
    count
    t

let rewrite_atom ~check_timeout demods count a =
  let args, count =
    List.fold_left
      (fun (acc, count) t ->
        check_timeout ();
        let t', count = rewrite_fix_term ~check_timeout demods count t in
        (t' :: acc, count))
      ([], count)
      a.args
  in
  ({ a with args = List.rev args }, count)

let rewrite_literal ~check_timeout demods count = function
  | Pos a ->
      let a', count = rewrite_atom ~check_timeout demods count a in
      (Pos a', count)
  | Neg a ->
      let a', count = rewrite_atom ~check_timeout demods count a in
      (Neg a', count)

let rewrite_clause ~check_timeout demods c =
  List.fold_left
    (fun (acc, count) lit ->
      check_timeout ();
      let lit', count = rewrite_literal ~check_timeout demods count lit in
      (lit' :: acc, count))
    ([], 0)
    c
  |> fun (lits, count) -> (List.rev lits, count)

let rewrite_atom_indexed ~check_timeout demod_index count a =
  let args, count =
    List.fold_left
      (fun (acc, count) t ->
        check_timeout ();
        let t', count =
          rewrite_fix_term_indexed ~check_timeout demod_index count t
        in
        (t' :: acc, count))
      ([], count)
      a.args
  in
  ({ a with args = List.rev args }, count)

let rewrite_literal_indexed ~check_timeout demod_index count = function
  | Pos a ->
      let a', count = rewrite_atom_indexed ~check_timeout demod_index count a in
      (Pos a', count)
  | Neg a ->
      let a', count = rewrite_atom_indexed ~check_timeout demod_index count a in
      (Neg a', count)

let rewrite_clause_indexed ~check_timeout demod_index c =
  List.fold_left
    (fun (acc, count) lit ->
      check_timeout ();
      let lit', count =
        rewrite_literal_indexed ~check_timeout demod_index count lit
      in
      (lit' :: acc, count))
    ([], 0)
    c
  |> fun (lits, count) -> (List.rev lits, count)

let oriented_demodulator_of_clause = function
  | [ Pos { pred = "="; args = [ l; r ] } ] ->
      Ordering.orient_equation l r
  | _ -> None

let resolve_pair c1 c2 i j =
  let l1 = List.nth c1 i in
  let l2 = List.nth c2 j in
  if sign l1 <> sign l2 then
    let a1 = atom_of_literal l1 in
    let a2 = atom_of_literal l2 in
    if a1.pred = a2.pred && List.length a1.args = List.length a2.args then
      try
        let s = unify_atoms a1 a2 empty_subst in
        let rem1 = List.filteri (fun k _ -> k <> i) c1 in
        let rem2 = List.filteri (fun k _ -> k <> j) c2 in
        simplify_clause (apply_subst_clause s (rem1 @ rem2))
      with Not_unifiable ->
        None
    else
      None
  else
    None

let resolve_two_clauses ~check_timeout ~emulate_v1 ~mode c1 c2 =
  let c1_renamed = rename_clause_apart c1 in
  let c2_renamed = rename_clause_apart c2 in
  let results = ref [] in
  let legacy_resolution_literals =
    match Sys.getenv_opt "IP_RESOLUTION_LITERAL_SELECTION" with
    | Some s -> String.equal (String.lowercase_ascii (String.trim s)) "legacy"
    | None ->
        begin
          match Sys.getenv_opt "IP_LEGACY_RESOLUTION_LITERALS" with
          | Some "1" | Some "true" | Some "TRUE" | Some "yes" | Some "YES" -> true
          | _ -> false
        end
  in
  let active_indices c =
    if legacy_resolution_literals then all_indices c
    else
      all_indices c
      |> List.filter (fun i -> is_active_literal ~emulate_v1 mode c i)
  in
  List.iter
    (fun i ->
      check_timeout ();
      List.iter
        (fun j ->
          check_timeout ();
          match resolve_pair c1_renamed c2_renamed i j with
          | Some c -> results := c :: !results
          | None -> ())
        (active_indices c2))
    (active_indices c1);
  dedup_clauses ~check_timeout !results

let factor_pairs_unrestricted ~check_timeout c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iter
    (fun i ->
      check_timeout ();
      List.iter
        (fun j ->
          check_timeout ();
          if i < j then
            let l1 = List.nth c i in
            let l2 = List.nth c j in
            if sign l1 = sign l2 then
              let a1 = atom_of_literal l1 in
              let a2 = atom_of_literal l2 in
              if a1.pred = a2.pred && List.length a1.args = List.length a2.args then
                try
                  let s = unify_atoms a1 a2 empty_subst in
                  let kept = List.filteri (fun k _ -> k <> j) c in
                  match simplify_clause (apply_subst_clause s kept) with
                  | Some c -> results := c :: !results
                  | None -> ()
                with Not_unifiable -> ())
        (all_indices c))
    (all_indices c);
  dedup_clauses ~check_timeout !results

let factor_pairs_ordered ~check_timeout ~emulate_v1 c =
  let c = rename_clause_apart c in
  let active = selected_or_maximal_indices ~emulate_v1 c in
  let results = ref [] in
  List.iter
    (fun i ->
      check_timeout ();
      List.iter
        (fun j ->
          check_timeout ();
          if i < j then
            let l1 = List.nth c i in
            let l2 = List.nth c j in
            if sign l1 = sign l2 then
              let a1 = atom_of_literal l1 in
              let a2 = atom_of_literal l2 in
              if a1.pred = a2.pred && List.length a1.args = List.length a2.args then
                try
                  let s = unify_atoms a1 a2 empty_subst in
                  let kept = List.filteri (fun k _ -> k <> j) c in
                  match simplify_clause (apply_subst_clause s kept) with
                  | Some c -> results := c :: !results
                  | None -> ()
                with Not_unifiable -> ())
        (all_indices c))
    active;
  dedup_clauses ~check_timeout !results

let factor_by_mode ~check_timeout ~emulate_v1 mode c =
  match mode with
  | Unrestricted -> factor_pairs_unrestricted ~check_timeout c
  | Ordered -> factor_pairs_ordered ~check_timeout ~emulate_v1 c
  | Ordered_with_fallback ->
      let ordered = factor_pairs_ordered ~check_timeout ~emulate_v1 c in
      if ordered <> [] then ordered
      else factor_pairs_unrestricted ~check_timeout c

let equality_resolution ~check_timeout ~emulate_v1 mode c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iteri
    (fun i lit ->
      check_timeout ();
      if is_active_literal ~emulate_v1 mode c i then
        match negative_equality lit with
        | None -> ()
        | Some (l, r) ->
            try
              let s = unify_terms l r empty_subst in
              let rest = List.filteri (fun j _ -> j <> i) c in
              match simplify_clause (apply_subst_clause s rest) with
              | Some c -> results := c :: !results
              | None -> ()
            with Not_unifiable -> ())
    c;
  dedup_clauses ~check_timeout !results

let equality_factoring ~check_timeout ~emulate_v1 mode c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iteri
    (fun i lit1 ->
      check_timeout ();
      if is_active_literal ~emulate_v1 mode c i then
        match positive_equality lit1 with
        | None -> ()
        | Some (l1, r1) ->
            List.iteri
              (fun j lit2 ->
                check_timeout ();
                if i < j then
                  match positive_equality lit2 with
                  | None -> ()
                  | Some (l2, r2) ->
                      try
                        let s = unify_terms l1 l2 empty_subst in
                        let rest = List.filteri (fun k _ -> k <> i) c in
                        let diseq = Neg { pred = "="; args = [ r1; r2 ] } in
                        match simplify_clause (apply_subst_clause s (diseq :: rest)) with
                        | Some c -> results := c :: !results
                        | None -> ()
                      with Not_unifiable -> ())
              c)
    c;
  dedup_clauses ~check_timeout !results

let rec non_variable_subterms ?(check_timeout = fun () -> ()) ?(path = []) t =
  check_timeout ();
  match t with
  | Var _ -> []
  | Fun (f, args) ->
      (path, t)
      :: List.concat
           (List.mapi
              (fun i a ->
                non_variable_subterms ~check_timeout ~path:(path @ [ i ]) a)
              args)

let oriented_sides mode l r =
  match mode with
  | Ordered ->
      begin
        match Ordering.compare_term_kbo l r with
        | 1 -> [ (l, r) ]
        | -1 -> [ (r, l) ]
        | _ -> []
      end
  | Unrestricted | Ordered_with_fallback ->
      begin
        match Ordering.compare_term_kbo l r with
        | 1 -> [ (l, r) ]
        | -1 -> [ (r, l) ]
        | _ -> [ (l, r); (r, l) ]
      end

let rec replace_term_at_path t path t' =
  match path with
  | [] -> t'
  | i :: rest ->
      match t with
      | Var _ -> t
      | Fun (f, args) ->
          let args' =
            List.mapi (fun j a -> if i = j then replace_term_at_path a rest t' else a) args
          in
          Fun (f, args')

let replace_atom_arg_at_path a arg_idx path t' =
  let args' =
    List.mapi (fun i t -> if i = arg_idx then replace_term_at_path t path t' else t) a.args
  in
  { a with args = args' }

let replace_literal_arg_at_path lit arg_idx path t' =
  match lit with
  | Pos a -> Pos (replace_atom_arg_at_path a arg_idx path t')
  | Neg a -> Neg (replace_atom_arg_at_path a arg_idx path t')

let superpose_from_into_position
    source
    target
    eq_index
    lhs
    rhs
    (te : Term_index.term_entry) =
  let source_rest = List.filteri (fun i _ -> i <> eq_index) source in
  let lit = List.nth target te.Term_index.lit_index in
  try
    let s = unify_terms lhs te.Term_index.subterm empty_subst in
    let rhs' = apply_subst_term s rhs in
    let lit' =
      replace_literal_arg_at_path
        lit
        te.Term_index.arg_index
        te.Term_index.path
        rhs'
    in
    let target' =
      List.mapi
        (fun i x -> if i = te.Term_index.lit_index then lit' else x)
        target
    in
    simplify_clause (apply_subst_clause s (source_rest @ target'))
  with Not_unifiable ->
    None

let indexed_superpose_given ~check_timeout ~emulate_v1 ~mode ~term_index ~all_by_id given =
  let results = ref [] in

  (* Direction 1 :
     égalités du given -> sous-termes des clauses indexées *)
  let given_source_clause = rename_clause_apart given.clause_d in

  List.iteri
    (fun eq_index lit ->
      check_timeout ();

      if is_active_literal ~emulate_v1 mode given_source_clause eq_index then
        match positive_equality lit with
        | None -> ()
        | Some (l, r) ->
            List.iter
              (fun (lhs, rhs) ->
                check_timeout ();

                let entries =
                  Term_index.find_terms_unifiable_with term_index lhs
                in

                List.iter
                  (fun (te : Term_index.term_entry) ->
                    check_timeout ();

                    if te.Term_index.clause_id <> given.id then
                      match Hashtbl.find_opt all_by_id te.Term_index.clause_id with
                      | None -> ()
                      | Some target_d ->
                          if is_active_literal ~emulate_v1 mode target_d.clause_d te.Term_index.lit_index then
                            let target_clause =
                              rename_clause_apart target_d.clause_d
                            in
                            match
                              superpose_from_into_position
                                given_source_clause
                                target_clause
                                eq_index
                                lhs
                                rhs
                                te
                            with
                            | Some c ->
                                results := (c, [ given.id; target_d.id ]) :: !results
                            | None ->
                                ()
                  ) entries)
              (oriented_sides mode l r))
    given_source_clause;

  (* Direction 2 :
     égalités indexées -> sous-termes du given *)
  let given_target_clause = rename_clause_apart given.clause_d in

  List.iteri
    (fun lit_index lit ->
      check_timeout ();

      if is_active_literal ~emulate_v1 mode given_target_clause lit_index then
        let a = atom_of_literal lit in

        List.iteri
          (fun arg_index arg ->
            check_timeout ();

            let subterms =
              non_variable_subterms ~check_timeout arg
            in

            List.iter
              (fun (path, subterm) ->
                check_timeout ();

                let eqs =
                  Term_index.find_equalities_unifiable_with term_index subterm
                in

                List.iter
                  (fun (ee : Term_index.equality_entry) ->
                    check_timeout ();

                    if ee.Term_index.clause_id <> given.id then
                      match Hashtbl.find_opt all_by_id ee.Term_index.clause_id with
                      | None -> ()
                      | Some eq_d ->
                          if is_active_literal ~emulate_v1 mode eq_d.clause_d ee.Term_index.lit_index then
                            let source_clause =
                              rename_clause_apart eq_d.clause_d
                            in
                            let te : Term_index.term_entry =
                              {
                                Term_index.clause_id = given.id;
                                lit_index;
                                arg_index;
                                path;
                                subterm;
                              }
                            in
                            match
                              superpose_from_into_position
                                source_clause
                                given_target_clause
                                ee.Term_index.lit_index
                                ee.Term_index.lhs
                                ee.Term_index.rhs
                                te
                            with
                            | Some c ->
                                results := (c, [ eq_d.id; given.id ]) :: !results
                            | None ->
                                ()
                  ) eqs)
              subterms)
          a.args)
    given_target_clause;

  dedup_clauses_with_parents ~check_timeout (List.rev !results)

let term_index_orientation_mode = function
  | Ordered -> Term_index.Oriented_only
  | Unrestricted | Ordered_with_fallback -> Term_index.Both_if_unorientable

let run_resolution_sos ?(limits = default_limits) ?(expensive_simplifications = true) ?(emulate_v1 = false) ~mode ~axioms ~support () =
  let start_t = Unix.gettimeofday () in

  let known : (string, int) Hashtbl.t = Hashtbl.create 4099 in
  let all_by_id : (int, derived) Hashtbl.t = Hashtbl.create 4099 in
  let active : derived list ref = ref [] in
  let passive : passive_entry list ref = ref [] in
  let all : derived list ref = ref [] in
  let demodulators : (term * term) list ref = ref [] in
  let demod_index = create_demod_index () in

  let literal_index = Discrimination_index.create () in
  let term_index = Term_index.create () in
  let fv_index = Feature_vector.create () in
  let simpl_fv_index = Feature_vector.create () in

  let indexed_demodulation_enabled =
    match Sys.getenv_opt "IP_INDEXED_DEMODULATION" with
    | Some s ->
        begin
          match String.lowercase_ascii (String.trim s) with
          | "1" | "true" | "yes" | "on" -> true
          | "0" | "false" | "no" | "off" -> false
          | _ -> true
        end
    | None -> true
  in

  let generated = ref 0 in
  let processed = ref 0 in
  let resolution_inferences = ref 0 in
  let factoring_inferences = ref 0 in
  let equality_resolution_inferences = ref 0 in
  let equality_factoring_inferences = ref 0 in
  let superposition_inferences = ref 0 in
  let demodulation_rewrites = ref 0 in
  let subsumption_tests = ref 0 in
  let subsumption_rejections = ref 0 in

  let context_by_id : (int, context) Hashtbl.t = Hashtbl.create 4099 in
  let split_var_by_clause : (string, split_var) Hashtbl.t = Hashtbl.create 257 in
  let sat = Prop_sat.create () in
  let sat_unsat = ref false in
  let current_model : (split_var, bool) Hashtbl.t option ref = ref None in
  let context_sat_cache : (string, bool) Hashtbl.t = Hashtbl.create 127 in

  let avatar_split_vars_used = ref 0 in
  let avatar_split_attempts = ref 0 in
  let avatar_successful_splits = ref 0 in
  let avatar_split_components = ref 0 in
  let avatar_split_rejected_disabled = ref 0 in
  let avatar_split_rejected_short = ref 0 in
  let avatar_split_rejected_equality = ref 0 in
  let avatar_split_rejected_nonground = ref 0 in
  let avatar_split_rejected_trivial = ref 0 in
  let avatar_split_rejected_quota = ref 0 in
  let avatar_contextual_empty_conflicts = ref 0 in
  let avatar_sat_clauses_added = ref 0 in
  let avatar_sat_solves = ref 0 in
  let avatar_sat_conflicts = ref 0 in
  let avatar_context_sat_tests = ref 0 in
  let avatar_context_sat_failures = ref 0 in
  let avatar_filtered_inferences = ref 0 in

  let next_passive_id = ref 0 in
  let given_count = ref 0 in

  let old_alarm_handler = ref None in
  let alarm_installed = ref false in

  let install_alarm () =
    match limits.time_limit_s with
    | None -> ()
    | Some t ->
        old_alarm_handler :=
          Some
            (Sys.signal
               Sys.sigalrm
               (Sys.Signal_handle (fun _ -> raise Timeout_hit)));
        alarm_installed := true;
        ignore (Unix.alarm (max 1 (int_of_float (ceil t))))
  in

  let cleanup_alarm () =
    if !alarm_installed then begin
      ignore (Unix.alarm 0);
      match !old_alarm_handler with
      | None -> ()
      | Some h -> Sys.set_signal Sys.sigalrm h
    end
  in

  let time_exceeded () =
    option_exists
      (fun t -> Unix.gettimeofday () -. start_t >= t)
      limits.time_limit_s
  in

  let check_timeout () =
    if time_exceeded () then raise Timeout_hit
  in

  let rewrite_with_demodulators c =
    if indexed_demodulation_enabled then
      rewrite_clause_indexed ~check_timeout demod_index c
    else
      rewrite_clause ~check_timeout !demodulators c
  in

  let clause_limit_exceeded () =
    option_exists
      (fun n -> !generated >= n)
      limits.max_generated_clauses
  in

  let rec term_weight = function
    | Var _ -> 1
    | Fun (_, args) ->
        1 + List.fold_left (fun acc t -> acc + term_weight t) 0 args
  in

  let atom_weight a =
    1 + List.fold_left (fun acc t -> acc + term_weight t) 0 a.args
  in

  let literal_weight lit =
    1 + atom_weight (atom_of_literal lit)
  in

  let clause_has_negative c =
    List.exists is_negative c
  in

  let clause_has_equality c =
    clause_contains_equality c
  in

  let rec symbols_of_term acc = function
    | Var _ -> acc
    | Fun (f, args) ->
        List.fold_left symbols_of_term (Types.StringSet.add f acc) args
  in

  let symbols_of_literal acc = function
    | Pos a | Neg a ->
        List.fold_left symbols_of_term (Types.StringSet.add a.pred acc) a.args
  in

  let symbols_of_clause c =
    List.fold_left symbols_of_literal Types.StringSet.empty c
  in

  let support_symbols =
    support
    |> List.fold_left
         (fun acc c -> Types.StringSet.union acc (symbols_of_clause c))
         Types.StringSet.empty
  in

  let symbol_overlap_count c =
    if Types.StringSet.is_empty support_symbols then 0
    else
      Types.StringSet.cardinal
        (Types.StringSet.inter support_symbols (symbols_of_clause c))
  in

  let equality_literal_count c =
    List.fold_left
      (fun n lit -> if literal_contains_equality lit then n + 1 else n)
      0
      c
  in

  let negative_equality_count c =
    List.fold_left
      (fun n lit ->
        match lit with
        | Neg { pred = "="; args = [ _; _ ] } -> n + 1
        | _ -> n)
      0
      c
  in

  let unit_positive_equality c =
    match c with
    | [ Pos { pred = "="; args = [ _; _ ] } ] -> true
    | _ -> false
  in

  let getenv_bool name default =
    match Sys.getenv_opt name with
    | None | Some "" -> default
    | Some "0" | Some "false" | Some "False" | Some "no" | Some "NO" -> false
    | Some _ -> true
  in

  let getenv_int name default =
    match Sys.getenv_opt name with
    | None -> default
    | Some s -> (try max 0 (int_of_string s) with Failure _ -> default)
  in

  let parse_aw_ratio name default_age default_weight =
    match Sys.getenv_opt name with
    | None | Some "" -> (default_age, default_weight)
    | Some s ->
        begin
          match String.split_on_char ':' s with
          | [ a; w ] ->
              begin
                try (max 0 (int_of_string a), max 0 (int_of_string w))
                with Failure _ -> (default_age, default_weight)
              end
          | [ w ] ->
              begin
                try (default_age, max 0 (int_of_string w))
                with Failure _ -> (default_age, default_weight)
              end
          | _ -> (default_age, default_weight)
        end
  in

  let initial_clause_count = List.length axioms + List.length support in

  let passive_selection =
    match Sys.getenv_opt "IP_PASSIVE_SELECTION" with
    | Some s when String.trim s <> "" -> String.lowercase_ascii (String.trim s)
    | Some _ | None ->
        let syn_min = getenv_int "IP_PASSIVE_SYN_MIN_CLAUSES" 40 in
        let syn_max = getenv_int "IP_PASSIVE_SYN_MAX_CLAUSES" 55 in
        if (not emulate_v1) && expensive_simplifications
           && initial_clause_count >= syn_min && initial_clause_count <= syn_max then
          "syn"
        else
          "classic"
  in

  let default_passive_weight_ratio =
    if (initial_clause_count >= 20 && initial_clause_count <= 24)
       || (initial_clause_count >= 56 && initial_clause_count <= 60) then
      4
    else
      10
  in
  let passive_age_ratio, passive_weight_ratio =
    if expensive_simplifications then
      parse_aw_ratio "IP_PASSIVE_AW_RATIO" 1 default_passive_weight_ratio
    else
      parse_aw_ratio "IP_PASSIVE_AW_RATIO" 1 default_passive_weight_ratio
  in
  let passive_age_ratio = if passive_age_ratio = 0 && passive_weight_ratio = 0 then 1 else passive_age_ratio in
  let passive_weight_ratio = if passive_age_ratio = 0 && passive_weight_ratio = 0 then 1 else passive_weight_ratio in
  let passive_balance = ref 0 in

  let avatar_enabled =
    (not emulate_v1) && expensive_simplifications && getenv_bool "IP_AVATAR_SPLITTING" false
  in
  let avatar_keep_original = getenv_bool "IP_AVATAR_KEEP_ORIGINAL" true in
  let avatar_ground_only = getenv_bool "IP_AVATAR_GROUND_ONLY" true in
  let avatar_split_on_given = getenv_bool "IP_AVATAR_SPLIT_ON_GIVEN" false in
  (* Split only larger clauses by default; small splits slowed down easy SYN proofs. *)
  let avatar_min_split_literals = getenv_int "IP_AVATAR_MIN_SPLIT" 6 in
  (* Keep AVATAR deliberately tiny by default: larger budgets delay easy SYN proofs. *)
  let avatar_max_split_vars = getenv_int "IP_AVATAR_MAX_SPLIT_VARS" 4 in
  let avatar_max_split_vars_per_clause =
    let default_per_clause = max 2 (avatar_max_split_vars / 2) in
    getenv_int "IP_AVATAR_MAX_SPLIT_VARS_PER_CLAUSE" default_per_clause
  in
  let avatar_max_component_percent = getenv_int "IP_AVATAR_MAX_COMPONENT_PERCENT" 80 in
  (* Context clauses are useful, but they should not starve the classical path. *)
  let avatar_context_penalty = getenv_int "IP_AVATAR_CONTEXT_PENALTY" 64 in
  let avatar_model_false_first =
    getenv_bool "IP_AVATAR_MODEL_FALSE_FIRST" true
  in
  let fast_condensation_enabled =
    (not emulate_v1) && expensive_simplifications && getenv_bool "IP_FAST_CONDENSATION" true
  in
  let full_condensation_max_input_clauses =
    getenv_int "IP_FULL_CONDENSATION_MAX_INPUT_CLAUSES" 32
  in
  let full_condensation_enabled =
    (not emulate_v1)
    && expensive_simplifications
    && getenv_bool "IP_FULL_CONDENSATION" false
    && initial_clause_count <= full_condensation_max_input_clauses
  in
  let contextual_literal_cutting_enabled =
    (not emulate_v1) && expensive_simplifications && getenv_bool "IP_CONTEXTUAL_LITERAL_CUTTING" false
  in
  let forward_subsumption_resolution_enabled =
    (not emulate_v1) && expensive_simplifications && getenv_bool "IP_FORWARD_SUBSUMPTION_RESOLUTION" true
  in
  let simplification_set_index_enabled =
    (not emulate_v1)
    && expensive_simplifications
    && getenv_bool "IP_SIMPLIFICATION_SET_INDEX" false
  in
  let simplification_set_max_clause_len =
    getenv_int "IP_SIMPLIFICATION_SET_MAX_CLAUSE_LEN" 8
  in
  let simplification_set_unit_only =
    getenv_bool "IP_SIMPLIFICATION_SET_UNIT_ONLY" true
  in
  let simplification_set_negative_max_len =
    getenv_int "IP_SIMPLIFICATION_SET_NEGATIVE_MAX_LEN" 3
  in
  let simplification_set_goal_max_len =
    getenv_int "IP_SIMPLIFICATION_SET_GOAL_MAX_LEN" 5
  in
  let simplification_set_equality_unit =
    getenv_bool "IP_SIMPLIFICATION_SET_EQUALITY_UNIT" false
  in
  let backward_passive_demodulation_enabled =
    (not emulate_v1)
    && expensive_simplifications
    && getenv_bool "IP_BACKWARD_PASSIVE_DEMODULATION" true
  in
  let backward_passive_demodulation_limit =
    getenv_int "IP_BACKWARD_PASSIVE_DEMODULATION_LIMIT" 256
  in
  let legacy_candidate_pool =
    (not emulate_v1) && getenv_bool "IP_LEGACY_CANDIDATE_POOL" false
  in
  let legacy_subsumption =
    (not emulate_v1) && getenv_bool "IP_LEGACY_SUBSUMPTION" false
  in
  let simplify_clause_modern c =
    if full_condensation_enabled then full_condense_clause c
    else if fast_condensation_enabled then fast_condense_clause c
    else simplify_clause c
  in

  let rec vars_of_term acc = function
    | Var v -> if List.exists (( = ) v) acc then acc else v :: acc
    | Fun (_, args) -> List.fold_left vars_of_term acc args
  in
  let vars_of_literal lit =
    let atom = atom_of_literal lit in
    List.fold_left vars_of_term [] atom.args |> List.sort_uniq String.compare
  in
  let literal_is_ground lit = vars_of_literal lit = [] in
  let clause_is_ground c = List.for_all literal_is_ground c in

  let disjoint xs ys =
    not (List.exists (fun x -> List.exists (( = ) x) ys) xs)
  in

  let split_components c =
    let rec add_component lit vars = function
      | [] -> [ ([ lit ], vars) ]
      | (lits, vs) :: rest ->
          if disjoint vars vs then (lits, vs) :: add_component lit vars rest
          else (lit :: lits, List.sort_uniq String.compare (vars @ vs)) :: rest
    in
    c
    |> List.fold_left (fun acc lit -> add_component lit (vars_of_literal lit) acc) []
    |> List.map (fun (lits, _) -> List.rev lits)
    |> List.rev
  in

  let context_of d =
    match Hashtbl.find_opt context_by_id d.id with
    | None -> []
    | Some ctx -> ctx
  in

  let context_key ctx = string_of_context (normalize_context ctx) in

  let split_component_key c = string_of_clause (normalize_clause c) in

  let split_var_for_component c =
    let key = split_component_key c in
    match Hashtbl.find_opt split_var_by_clause key with
    | Some v -> v
    | None ->
        let v = Prop_sat.new_var sat in
        Hashtbl.add split_var_by_clause key v;
        incr avatar_split_vars_used;
        v
  in

  let refresh_avatar_model () =
    if avatar_enabled then begin
      incr avatar_sat_solves;
      match Prop_sat.model ~prefer_false:avatar_model_false_first sat [] with
      | Some model ->
          current_model := Some model;
          sat_unsat := false
      | None ->
          current_model := None;
          sat_unsat := true;
          incr avatar_sat_conflicts
    end
  in

  let sat_add_clause clause =
    incr avatar_sat_clauses_added;
    Hashtbl.clear context_sat_cache;
    Prop_sat.add_clause sat clause;
    refresh_avatar_model ()
  in

  let sat_context_sat ctx =
    let ctx = normalize_context ctx in
    if ctx = [] then true
    else begin
      incr avatar_context_sat_tests;
      let key = context_key ctx in
      match Hashtbl.find_opt context_sat_cache key with
      | Some ok -> ok
      | None ->
          incr avatar_sat_solves;
          let ok = Prop_sat.satisfiable sat ctx in
          Hashtbl.replace context_sat_cache key ok;
          if not ok then begin
            incr avatar_context_sat_failures;
            incr avatar_sat_conflicts
          end;
          ok
    end
  in

  let avatar_context_enabled ctx =
    let ctx = normalize_context ctx in
    if ctx = [] || not avatar_enabled then true
    else
      match !current_model with
      | None -> false
      | Some model ->
          List.for_all (Prop_sat.lit_true_in_model model) ctx
  in

  let derived_enabled d = avatar_context_enabled (context_of d) in

  let first_order_compatible d1 d2 =
    if not avatar_enabled then true
    else
      let ctx = normalize_context (context_of d1 @ context_of d2) in
      if ctx = [] then true
      else
        let ok = avatar_context_enabled ctx && sat_context_sat ctx in
        if not ok then incr avatar_filtered_inferences;
        ok
  in

  let should_split c =
    incr avatar_split_attempts;
    if not avatar_enabled then begin
      incr avatar_split_rejected_disabled;
      None
    end else if List.length c < avatar_min_split_literals then begin
      incr avatar_split_rejected_short;
      None
    end else if clause_has_equality c then begin
      incr avatar_split_rejected_equality;
      None
    end else if avatar_ground_only && not (clause_is_ground c) then begin
      incr avatar_split_rejected_nonground;
      None
    end else
      match split_components c with
      | [] | [ _ ] ->
          incr avatar_split_rejected_trivial;
          None
      | comps ->
          let total_lits = List.length c in
          let largest_component =
            comps |> List.map List.length |> List.fold_left max 0
          in
          let too_unbalanced =
            largest_component * 100 > total_lits * avatar_max_component_percent
          in
          let new_needed =
            comps
            |> List.map split_component_key
            |> List.sort_uniq String.compare
            |> List.fold_left
                 (fun acc key -> if Hashtbl.mem split_var_by_clause key then acc else acc + 1)
                 0
          in
          if too_unbalanced then begin
            incr avatar_split_rejected_trivial;
            None
          end else if new_needed > avatar_max_split_vars_per_clause then begin
            incr avatar_split_rejected_quota;
            None
          end else if !avatar_split_vars_used + new_needed > avatar_max_split_vars then begin
            incr avatar_split_rejected_quota;
            None
          end else Some comps
  in

  let clause_weight c =
    let len = List.length c in
    let base =
      List.fold_left (fun acc lit -> acc + literal_weight lit) 0 c
    in
    if emulate_v1 then
      let unit_bonus = if len = 1 then -2 else 0 in
      let negative_bonus = if clause_has_negative c then -1 else 0 in
      let equality_penalty = if clause_has_equality c then 2 else 0 in
      max 1 (base + unit_bonus + negative_bonus + equality_penalty)
    else
      (* Vampire strongly penalizes long clauses to fight combinatorial explosion *)
      let length_penalty =
        if len > 3 then (len - 3) * 5 else 0
      in
      let unit_bonus =
        if len = 1 then -3 else 0
      in
      (* Favor clauses with negative literals as they are often derived from the conjecture *)
      let negative_bonus =
        if clause_has_negative c then -2 else 0
      in
      let equality_penalty =
        if clause_has_equality c then 2 else 0
      in
      max 1 (base + length_penalty + unit_bonus + negative_bonus + equality_penalty)
  in

  let make_result stop_reason =
    {
      stop_reason;
      derivation = List.rev !all;
      stats = {
        generated_clauses = !generated;
        processed_clauses = !processed;
        resolution_inferences = !resolution_inferences;
        factoring_inferences = !factoring_inferences;
        equality_resolution_inferences = !equality_resolution_inferences;
        equality_factoring_inferences = !equality_factoring_inferences;
        superposition_inferences = !superposition_inferences;
        demodulation_rewrites = !demodulation_rewrites;
        subsumption_tests = !subsumption_tests;
        subsumption_rejections = !subsumption_rejections;
        avatar_enabled;
        avatar_keep_original;
        avatar_min_split_literals;
        avatar_max_split_vars;
        avatar_split_vars_used = !avatar_split_vars_used;
        avatar_split_attempts = !avatar_split_attempts;
        avatar_successful_splits = !avatar_successful_splits;
        avatar_split_components = !avatar_split_components;
        avatar_split_rejected_disabled = !avatar_split_rejected_disabled;
        avatar_split_rejected_short = !avatar_split_rejected_short;
        avatar_split_rejected_equality = !avatar_split_rejected_equality;
        avatar_split_rejected_nonground = !avatar_split_rejected_nonground;
        avatar_split_rejected_trivial = !avatar_split_rejected_trivial;
        avatar_split_rejected_quota = !avatar_split_rejected_quota;
        avatar_contextual_empty_conflicts = !avatar_contextual_empty_conflicts;
        avatar_sat_clauses_added = !avatar_sat_clauses_added;
        avatar_sat_solves = !avatar_sat_solves;
        avatar_sat_conflicts = !avatar_sat_conflicts;
        avatar_context_sat_tests = !avatar_context_sat_tests;
        avatar_context_sat_failures = !avatar_context_sat_failures;
        avatar_filtered_inferences = !avatar_filtered_inferences;
        wall_clock_s = Unix.gettimeofday () -. start_t;
      };
    }
  in

  let active_clauses () =
    !active
  in

  let is_subsumed_by ?(ignore_id = -1) ?(context = []) _ c =
    let context = normalize_context context in
    if emulate_v1 || legacy_subsumption then
      let c_norm = normalize_clause c in
      List.exists
        (fun d ->
          d.id <> ignore_id
          && context_subsumes (context_of d) context
          && List.for_all (fun lit -> List.exists (( = ) lit) c_norm) (normalize_clause d.clause_d))
        (active_clauses ())
    else
      let candidates =
        let global_candidates =
          Feature_vector.find_subsuming_candidates fv_index c
        in
        if simplification_set_index_enabled then
          unique_ids
            (global_candidates
             @ Feature_vector.find_subsuming_candidates simpl_fv_index c)
        else
          global_candidates
      in
      let rec aux = function
        | [] -> false
        | id :: tl ->
            if id = ignore_id then aux tl
            else match Hashtbl.find_opt all_by_id id with
            | None -> aux tl
            | Some d ->
                check_timeout ();
                incr subsumption_tests;
                if context_subsumes (context_of d) context && subsumes d.clause_d c then true else aux tl
      in
      aux candidates
  in

  let index_clause_for_inference d =
    Discrimination_index.add_clause literal_index ~clause_id:d.id d.clause_d;
    Term_index.add_clause
      term_index
      ~clause_id:d.id
      ~orientation_mode:(term_index_orientation_mode mode)
      d.clause_d
  in

  let delete_derived d =
    Hashtbl.remove all_by_id d.id;
    Feature_vector.remove fv_index d.id;
    Feature_vector.remove simpl_fv_index d.id;
    let key = context_key (context_of d) ^ "|" ^ string_of_clause d.clause_d in
    Hashtbl.remove known key;
    Hashtbl.remove context_by_id d.id
  in

  let enqueue_passive d =
    let context_penalty = if context_of d = [] then 0 else avatar_context_penalty in
    let entry =
      {
        passive_id = !next_passive_id;
        age = !next_passive_id;
        weight = clause_weight d.clause_d + context_penalty;
        d;
      }
    in
    incr next_passive_id;
    passive := entry :: !passive;
    Feature_vector.add fv_index d.clause_d d.id;
    if legacy_candidate_pool then
      index_clause_for_inference d
  in

  let rec add_clause ?(context = []) ?(allow_split = true) ~parents ~rule c =
    check_timeout ();
    let context = normalize_context context in
    if clause_limit_exceeded () || !sat_unsat || (avatar_enabled && context <> [] && not (sat_context_sat context)) then None
    else
      let c, rewrites =
        try rewrite_with_demodulators c
        with Rewrite_limit_hit -> (c, 0)
      in
      demodulation_rewrites := !demodulation_rewrites + rewrites;
      match simplify_clause_modern c with
      | None -> None
      | Some [] when context <> [] ->
          incr avatar_contextual_empty_conflicts;
          sat_add_clause (List.map neg_prop_lit context);
          None
      | Some c ->
          if allow_split && avatar_enabled && not avatar_split_on_given then
            match should_split c with
            | Some comps ->
                incr avatar_successful_splits;
                avatar_split_components := !avatar_split_components + List.length comps;
                let original =
                  if avatar_keep_original then
                    add_clause ~context ~allow_split:false ~parents ~rule:(rule ^ "/avatar_original") c
                  else None
                in
                let vars = List.map split_var_for_component comps in
                sat_add_clause (List.map neg_prop_lit context @ List.map (fun v -> PPos v) vars);
                List.iter2
                  (fun comp var ->
                    ignore (add_clause ~context:(PPos var :: context) ~allow_split:false ~parents ~rule:(rule ^ "/avatar_component") comp))
                  comps
                  vars;
                original
            | None -> add_clause ~context ~allow_split:false ~parents ~rule c
          else
            let key = context_key context ^ "|" ^ string_of_clause c in
            if Hashtbl.mem known key then None
            else if is_subsumed_by ~context (active_clauses ()) c then begin
              incr subsumption_rejections;
              None
            end
            else begin
              incr generated;
              let d = { id = next_id (); parents; rule; clause_d = c; is_active = false } in
              Hashtbl.add known key d.id;
              Hashtbl.replace context_by_id d.id context;
              Hashtbl.replace all_by_id d.id d;
              all := d :: !all;
              enqueue_passive d;
              Some d
            end
  in

  let simplify_selected d =
    check_timeout ();
    let c, rewrites =
      try rewrite_with_demodulators d.clause_d
      with Rewrite_limit_hit -> (d.clause_d, 0)
    in
    demodulation_rewrites := !demodulation_rewrites + rewrites;
    match simplify_clause_modern c with
    | None -> None
    | Some c ->
        let d_context = context_of d in
        if is_subsumed_by ~ignore_id:d.id ~context:d_context (active_clauses ()) c then begin
          incr subsumption_rejections;
          None
        end
        else begin
          if forward_subsumption_resolution_enabled then begin
            let contextual_candidates c_curr =
              if not contextual_literal_cutting_enabled then []
              else
                let len = List.length c_curr in
                let eqs = List.fold_left (fun n lit -> if literal_contains_equality lit then n + 1 else n) 0 c_curr in
                if len <= 1 || len > 8 || eqs > 3 then []
                else
                  c_curr
                  |> List.mapi (fun i lit -> i, lit)
                  |> List.fold_left
                       (fun acc (i, lit) ->
                         check_timeout ();
                         let test_clause = List.mapi (fun j lit' -> if i = j then negate_literal lit else lit') c_curr in
                         Feature_vector.find_subsuming_candidates fv_index test_clause @ acc)
                       []
            in
            let rec reduce c_curr =
              let candidates =
                unique_ids (Feature_vector.find_subsuming_candidates fv_index c_curr @ contextual_candidates c_curr)
              in
              let rec first_reduction = function
                | [] -> None
                | id :: tl ->
                    check_timeout ();
                    if id = d.id then first_reduction tl
                    else
                      match Hashtbl.find_opt all_by_id id with
                      | Some a when a.is_active && context_subsumes (context_of a) d_context ->
                          begin
                            match subsumption_resolution a.clause_d c_curr with
                            | None -> first_reduction tl
                            | Some c_new ->
                                if List.length c_new < List.length c_curr then
                                  simplify_clause_modern c_new
                                else
                                  first_reduction tl
                          end
                      | _ -> first_reduction tl
              in
              match first_reduction candidates with
              | None -> c_curr
              | Some c_next -> reduce c_next
            in
            let c_final = reduce c in

            if is_subsumed_by ~ignore_id:d.id ~context:d_context (active_clauses ()) c_final then begin
              incr subsumption_rejections;
              None
            end else if c_final = d.clause_d then
              Some d
            else begin
              let rule =
                if c <> d.clause_d then
                  "selected_simplification"
                else
                  "forward_subsumption_resolution"
              in
              let d' =
                {
                  id = next_id ();
                  parents = [ d.id ];
                  rule;
                  clause_d = c_final;
                  is_active = false;
                }
              in
              let key = context_key d_context ^ "|" ^ string_of_clause c_final in
              Hashtbl.replace known key d'.id;
              Hashtbl.replace context_by_id d'.id d_context;
              Hashtbl.replace all_by_id d'.id d';
              all := d' :: !all;
              Some d'
            end
          end else
            if c = d.clause_d then
              Some d
            else begin
              let d' =
                {
                  id = next_id ();
                  parents = [ d.id ];
                  rule = "selected_simplification";
                  clause_d = c;
                  is_active = false;
                }
              in
              let key = context_key d_context ^ "|" ^ string_of_clause c in
              Hashtbl.replace known key d'.id;
              Hashtbl.replace context_by_id d'.id d_context;
              Hashtbl.replace all_by_id d'.id d';
              all := d' :: !all;
              Some d'
            end
        end
  in

  let select_by_age entries =
    List.fold_left
      (fun best e ->
        match best with
        | None -> Some e
        | Some b -> if e.age < b.age then Some e else best)
      None entries
  in

  let select_by_weight entries =
    List.fold_left
      (fun best e ->
        match best with
        | None -> Some e
        | Some b ->
            if e.weight < b.weight || (e.weight = b.weight && e.age < b.age) then
              Some e
            else best)
      None entries
  in

  let rec term_vars acc = function
    | Var v -> if List.exists (( = ) v) acc then acc else v :: acc
    | Fun (_, args) -> List.fold_left term_vars acc args
  in

  let literal_vars acc lit =
    List.fold_left term_vars acc (atom_of_literal lit).args
  in

  let clause_var_count c =
    List.length (List.fold_left literal_vars [] c)
  in

  let scored_passive_score mode e =
    let c = e.d.clause_d in
    let len = List.length c in
    let vars = clause_var_count c in
    let age_relief = e.age / 32 in
    match mode with
    | "legacy" ->
        let negative_bonus = if clause_has_negative c then 500 else 0 in
        let equality_penalty = if clause_has_equality c then 25 else 0 in
        (len * 1000) + equality_penalty - negative_bonus + (e.age / 4)
    | "short" -> (len * 100) + e.weight + (vars * 8) - age_relief
    | "syn" ->
        let negative_bonus = if clause_has_negative c then 12 else 0 in
        let unit_bonus = if len = 1 then 32 else 0 in
        (len * 80) + e.weight + (vars * 12) - negative_bonus - unit_bonus - age_relief
    | "goal" | "goal-directed" ->
        let overlap = symbol_overlap_count c in
        let negative_bonus = if clause_has_negative c then 18 else 0 in
        let unit_bonus = if len = 1 then 45 else 0 in
        let goal_bonus = overlap * getenv_int "IP_PASSIVE_GOAL_SYMBOL_BONUS" 35 in
        (len * 70) + e.weight + (vars * 10) - goal_bonus - negative_bonus
        - unit_bonus - age_relief
    | "goal-short" ->
        let overlap = symbol_overlap_count c in
        let goal_bonus = overlap * getenv_int "IP_PASSIVE_GOAL_SYMBOL_BONUS" 35 in
        (len * 95) + e.weight + (vars * 10) - goal_bonus - age_relief
    | "equality" ->
        let eqs = equality_literal_count c in
        let neg_eqs = negative_equality_count c in
        let unit_eq_bonus = if unit_positive_equality c then 96 else 0 in
        let eq_bonus = min 48 (eqs * 14) in
        let neg_eq_bonus = min 36 (neg_eqs * 18) in
        let mixed_penalty = if eqs = 0 then 40 else 0 in
        (len * 70) + e.weight + (vars * 10) + mixed_penalty
        - unit_eq_bonus - eq_bonus - neg_eq_bonus - age_relief
    | "weight" -> e.weight - age_relief
    | _ -> e.weight - age_relief
  in

  let select_by_scored mode entries =
    List.fold_left
      (fun best e ->
        match best with
        | None -> Some e
        | Some b ->
            let se = scored_passive_score mode e in
            let sb = scored_passive_score mode b in
            if se < sb || (se = sb && e.age < b.age) then Some e else best)
      None entries
  in

  let split_commas s =
    s
    |> String.split_on_char ','
    |> List.map (fun x -> String.lowercase_ascii (String.trim x))
    |> List.filter (fun x -> x <> "")
  in

  let layered_schedule =
    match Sys.getenv_opt "IP_LAYERED_SELECTION" with
    | Some s when String.trim s <> "" -> split_commas s
    | Some _ | None ->
        [ "unit"; "equality"; "goal"; "short"; "age"; "weight" ]
  in

  let layered_cursor = ref 0 in

  let layer_entries layer entries =
    let keep e =
      let c = e.d.clause_d in
      match layer with
      | "unit" -> List.length c = 1
      | "negative-unit" | "neg-unit" ->
          List.length c = 1 && clause_has_negative c
      | "equality-unit" | "eq-unit" ->
          List.length c = 1 && clause_has_equality c
      | "equality" | "eq" -> clause_has_equality c
      | "goal" | "support" -> symbol_overlap_count c > 0
      | "short" -> List.length c <= getenv_int "IP_LAYERED_SHORT_MAX_LEN" 2
      | "negative" | "neg" -> clause_has_negative c
      | "age" | "weight" | "all" | "any" -> true
      | _ -> true
    in
    List.filter keep entries
  in

  let select_in_layer layer entries =
    match layer with
    | "age" -> select_by_age entries
    | "weight" | "all" | "any" -> select_by_weight entries
    | "equality" | "eq" | "equality-unit" | "eq-unit" ->
        select_by_scored "equality" entries
    | "goal" | "support" -> select_by_scored "goal" entries
    | "short" -> select_by_scored "short" entries
    | "unit" | "negative-unit" | "neg-unit" | "negative" | "neg" ->
        select_by_scored "syn" entries
    | _ -> select_by_weight entries
  in

  let select_by_layered entries =
    match layered_schedule with
    | [] -> select_by_weight entries
    | layers ->
        let n = List.length layers in
        let rec try_layer attempts =
          if attempts >= n then select_by_weight entries
          else
            let idx = (!layered_cursor + attempts) mod n in
            let layer = List.nth layers idx in
            let candidates = layer_entries layer entries in
            match candidates with
            | [] -> try_layer (attempts + 1)
            | _ ->
                layered_cursor := (idx + 1) mod n;
                select_in_layer layer candidates
        in
        try_layer 0
  in

  let remove_passive_entry selected =
    passive :=
      List.filter (fun e -> e.passive_id <> selected.passive_id) !passive
  in

  let select_given () =
    let rec aux () =
      match !passive with
      | [] -> None
      | entries ->
          let enabled_entries =
            if avatar_enabled then List.filter (fun e -> derived_enabled e.d) entries else entries
          in
          let selected =
            match enabled_entries with
            | [] -> None
            | entries when passive_selection = "layered" ->
                select_by_layered entries
            | entries when passive_selection <> "classic" ->
                select_by_scored passive_selection entries
            | entries ->
                let select_weight =
                    if passive_age_ratio = 0 then true
                    else if passive_weight_ratio = 0 then false
                    else if !passive_balance > 0 then true
                    else if !passive_balance < 0 then false
                    else passive_age_ratio <= passive_weight_ratio
                  in
                  if select_weight then begin
                    passive_balance := !passive_balance - passive_age_ratio;
                    select_by_weight entries
                  end else begin
                    passive_balance := !passive_balance + passive_weight_ratio;
                    select_by_age entries
                  end
          in
          match selected with
          | None -> None
          | Some e ->
              remove_passive_entry e;
              if Hashtbl.mem all_by_id e.d.id then (
                incr given_count;
                Some e.d
              ) else aux ()
    in
    aux ()
  in

  let backward_demodulate rule =
    let kept_active = ref [] in
    List.iter
      (fun d ->
        check_timeout ();
        let c', rewrites =
          try rewrite_clause ~check_timeout [ rule ] d.clause_d
          with Rewrite_limit_hit -> (d.clause_d, 0)
        in
        if rewrites > 0 then begin
          let d_context = context_of d in
          delete_derived d;
          match simplify_clause c' with
          | None -> ()
          | Some c_final ->
              ignore (add_clause ~context:d_context ~parents:[ d.id ] ~rule:"backward_demod" c_final)
        end
        else kept_active := d :: !kept_active)
      !active;
    active := List.rev !kept_active
  in

  let backward_demodulate_passive rule =
    if backward_passive_demodulation_enabled then begin
      let rewritten = ref 0 in
      let kept_passive = ref [] in
      List.iter
        (fun e ->
          check_timeout ();
          if !rewritten >= backward_passive_demodulation_limit then
            kept_passive := e :: !kept_passive
          else
            let d = e.d in
            let c', rewrites =
              try rewrite_clause ~check_timeout [ rule ] d.clause_d
              with Rewrite_limit_hit -> (d.clause_d, 0)
            in
            if rewrites > 0 then begin
              incr rewritten;
              let d_context = context_of d in
              delete_derived d;
              match simplify_clause_modern c' with
              | None -> ()
              | Some c_final ->
                  ignore
                    (add_clause
                       ~context:d_context
                       ~parents:[ d.id ]
                       ~rule:"backward_passive_demod"
                       c_final)
            end
            else
              kept_passive := e :: !kept_passive)
        !passive;
      passive := List.rev !kept_passive
    end
  in

  let register_demodulator c =
    match oriented_demodulator_of_clause c with
    | None -> ()
    | Some rule ->
        if not (List.exists (( = ) rule) !demodulators) then begin
          demodulators := rule :: !demodulators;
          add_demod_index demod_index rule;
          if expensive_simplifications then begin
            backward_demodulate rule;
            backward_demodulate_passive rule
          end
        end
  in

  let index_active_clause d =
    index_clause_for_inference d
  in

  let index_simplification_clause d =
    let c = d.clause_d in
    let len = List.length c in
    let should_index =
      if simplification_set_unit_only then
        (clause_has_negative c && len = 1)
        || (simplification_set_equality_unit && unit_positive_equality c)
      else
        len <= simplification_set_max_clause_len
        && ((clause_has_negative c
             && len <= simplification_set_negative_max_len)
            || (symbol_overlap_count c > 0
                && len <= simplification_set_goal_max_len)
            || (simplification_set_equality_unit
                && unit_positive_equality c))
    in
    if simplification_set_index_enabled && should_index then
      Feature_vector.add simpl_fv_index d.clause_d d.id
  in

  let backward_subsume given =
    if emulate_v1 || legacy_subsumption then
      let given_norm = normalize_clause given.clause_d in
      let kept_active = ref [] in
      List.iter
        (fun d ->
          check_timeout ();
          if d.id <> given.id then begin
            if List.for_all (fun lit -> List.exists (( = ) lit) (normalize_clause d.clause_d)) given_norm then
              delete_derived d
            else
              kept_active := d :: !kept_active
          end else
            kept_active := d :: !kept_active)
        !active;
      active := List.rev !kept_active
    else
      let candidates = Feature_vector.find_subsumed_candidates fv_index given.clause_d in
      if candidates = [] then ()
      else
        let candidates_set = Hashtbl.create (List.length candidates) in
        List.iter (fun id -> Hashtbl.add candidates_set id ()) candidates;

        let kept_active = ref [] in
        List.iter
          (fun d ->
            check_timeout ();
            if Hashtbl.mem candidates_set d.id && d.id <> given.id && (not avatar_enabled || derived_enabled d) then (
              incr subsumption_tests;
              if context_subsumes (context_of given) (context_of d) && subsumes given.clause_d d.clause_d then
                delete_derived d
              else
                kept_active := d :: !kept_active
            ) else
              kept_active := d :: !kept_active)
          !active;
        active := List.rev !kept_active;

        let kept_passive = ref [] in
        List.iter
          (fun e ->
            check_timeout ();
            if Hashtbl.mem candidates_set e.d.id && e.d.id <> given.id && (not avatar_enabled || derived_enabled e.d) then (
              incr subsumption_tests;
              if context_subsumes (context_of given) (context_of e.d) && subsumes given.clause_d e.d.clause_d then
                delete_derived e.d
              else
                kept_passive := e :: !kept_passive
            ) else
              kept_passive := e :: !kept_passive)
          !passive;
        passive := List.rev !kept_passive
  in

  let activate given =
    backward_subsume given;
    active := given :: !active;
    index_simplification_clause given;
    if context_of given = [] then register_demodulator given.clause_d;
    index_active_clause given
  in

  let resolution_candidates ~emulate_v1 given =
    let indexed_ids = Hashtbl.create 127 in
    let given_active_indices = selected_or_maximal_indices ~emulate_v1 given.clause_d in

    List.iter
      (fun i ->
        let lit = List.nth given.clause_d i in
        check_timeout ();
        let entries =
          Discrimination_index.find_complementary literal_index lit
        in
        List.iter
          (fun e ->
            check_timeout ();
            if e.Discrimination_index.clause_id <> given.id then
              match Hashtbl.find_opt all_by_id e.Discrimination_index.clause_id with
              | Some d ->
                  if (not avatar_enabled || derived_enabled d)
                     && is_active_literal ~emulate_v1 mode d.clause_d e.Discrimination_index.lit_index then
                    Hashtbl.replace indexed_ids d.id ()
              | None -> ())
          entries)
      given_active_indices;

    let indexed =
      Hashtbl.fold
        (fun id () acc ->
          match Hashtbl.find_opt all_by_id id with
          | Some d when first_order_compatible given d -> d :: acc
          | Some _ -> acc
          | None -> acc)
        indexed_ids
        []
    in

    let fallback =
      List.fold_left
        (fun acc d ->
          check_timeout ();
          if d.id <> given.id && (not avatar_enabled || derived_enabled d) && not (Hashtbl.mem indexed_ids d.id) && first_order_compatible given d then
            d :: acc
          else
            acc)
        []
        !active
    in

    indexed @ fallback
  in

  let context_of_parent_ids parents =
    parents
    |> List.fold_left
         (fun acc id ->
           match Hashtbl.find_opt all_by_id id with
           | None -> acc
           | Some d -> context_of d @ acc)
         []
    |> normalize_context
  in

  let add_generated ?context ~parents ~rule c stop_reason =
    check_timeout ();
    if !stop_reason = None then begin
      let context =
        match context with
        | None -> context_of_parent_ids parents
        | Some ctx -> normalize_context ctx
      in
      match add_clause ~context ~parents ~rule c with
      | Some d when d.clause_d = [] ->
          stop_reason := Some (Refutation_found d)
      | _ -> ()
    end
  in

  try
    install_alarm ();

    let stop_reason = ref None in

    let add_initial c =
      match add_clause ~parents:[] ~rule:"input" c with
      | Some d when d.clause_d = [] ->
          stop_reason := Some (Refutation_found d)
      | _ -> ()
    in

    List.iter
      (fun c ->
        check_timeout ();
        if !stop_reason = None then add_initial c)
      axioms;

    List.iter
      (fun c ->
        check_timeout ();
        if !stop_reason = None then add_initial c)
      support;

    while !stop_reason = None do
      check_timeout ();

      if !sat_unsat then
        stop_reason := Some (Refutation_found { id = next_id (); parents = []; rule = "avatar_sat_conflict"; clause_d = []; is_active = false })
      else if clause_limit_exceeded () then
        stop_reason := Some Clause_limit
      else
        match select_given () with
        | None ->
            stop_reason := Some Saturation
        | Some given0 ->
            if Hashtbl.mem all_by_id given0.id then begin
              match simplify_selected given0 with
              | None -> ()
              | Some given ->
                  incr processed;

                  if given.clause_d = [] then
                    stop_reason := Some (Refutation_found given)
                  else begin
                    if avatar_enabled && avatar_split_on_given then begin
                      match should_split given.clause_d with
                      | None -> ()
                      | Some comps ->
                          incr avatar_successful_splits;
                          avatar_split_components := !avatar_split_components + List.length comps;
                          let vars = List.map split_var_for_component comps in
                          sat_add_clause (List.map neg_prop_lit (context_of given) @ List.map (fun v -> PPos v) vars);
                          List.iter2
                            (fun comp var ->
                              ignore (add_clause ~context:(PPos var :: context_of given) ~allow_split:false ~parents:[ given.id ] ~rule:"avatar_given_component" comp))
                            comps
                            vars
                    end;
                    activate given;

                    List.iter
                      (fun fc ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr factoring_inferences;
                          add_generated
                            ~parents:[ given.id ]
                            ~rule:
                              (match mode with
                               | Unrestricted -> "factor"
                               | Ordered -> "ordered_factor"
                               | Ordered_with_fallback ->
                                   "ordered_factor/fallback")
                            fc
                            stop_reason
                        end)
                      (factor_by_mode ~check_timeout ~emulate_v1 mode given.clause_d);

                    List.iter
                      (fun c ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr equality_resolution_inferences;
                          add_generated
                            ~parents:[ given.id ]
                            ~rule:"equality_resolution"
                            c
                            stop_reason
                        end)
                      (equality_resolution ~check_timeout ~emulate_v1 mode given.clause_d);

                    List.iter
                      (fun c ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr equality_factoring_inferences;
                          add_generated
                            ~parents:[ given.id ]
                            ~rule:"equality_factoring"
                            c
                            stop_reason
                        end)
                      (equality_factoring ~check_timeout ~emulate_v1 mode given.clause_d);

                    List.iter
                      (fun other ->
                        check_timeout ();
                        if !stop_reason = None && other.id <> given.id then begin
                          let resolvents =
                            resolve_two_clauses
                              ~check_timeout
                              ~emulate_v1
                              ~mode
                              given.clause_d
                              other.clause_d
                          in
                          List.iter
                            (fun r ->
                              check_timeout ();
                              if !stop_reason = None then begin
                                incr resolution_inferences;
                                add_generated
                                  ~parents:[ given.id; other.id ]
                                  ~rule:"resolution"
                                  r
                                  stop_reason
                              end)
                            resolvents
                        end)
                      (resolution_candidates ~emulate_v1 given);

                    List.iter
                      (fun (r, parents) ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr superposition_inferences;
                          add_generated
                            ~parents
                            ~rule:"indexed_superposition"
                            r
                            stop_reason
                        end)
                      (indexed_superpose_given
                         ~check_timeout
                         ~emulate_v1
                         ~mode
                         ~term_index
                         ~all_by_id
                         given)
                  end
            end
    done;

    let final_reason =
      match !stop_reason with
      | Some r -> r
      | None -> Saturation
    in
    let result = make_result final_reason in
    cleanup_alarm ();
    result

  with
  | Timeout_hit ->
      let result = make_result Time_limit in
      cleanup_alarm ();
      result
  | exn ->
      cleanup_alarm ();
      raise exn

let run_resolution ?(limits = default_limits) ~mode clauses =
  run_resolution_sos ~limits ~mode ~axioms:[] ~support:clauses ()

let print_derivation deriveds =
  List.iter
    (fun d ->
      let parents =
        match d.parents with
        | [] -> "input"
        | ps -> String.concat "," (List.map string_of_int ps)
      in
      Printf.printf
        "%4d. %-30s [%s] %s\n"
        d.id
        d.rule
        parents
        (string_of_clause d.clause_d))
    deriveds

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
    | 'A' .. 'Z' -> v
    | '_' -> v
    | _ -> String.capitalize_ascii v
  else
    "V"

let rec tptp_term = function
  | Var v -> tptp_var_name v
  | Fun (f, []) -> tptp_name f
  | Fun (f, args) ->
      Printf.sprintf
        "%s(%s)"
        (tptp_name f)
        (String.concat "," (List.map tptp_term args))

let tptp_atom a =
  match a.pred, a.args with
  | "=", [lhs; rhs] ->
      Printf.sprintf "%s = %s" (tptp_term lhs) (tptp_term rhs)
  | _, [] -> tptp_name a.pred
  | _ ->
      Printf.sprintf
        "%s(%s)"
        (tptp_name a.pred)
        (String.concat "," (List.map tptp_term a.args))

let tptp_literal = function
  | Pos a -> tptp_atom a
  | Neg { pred = "="; args = [lhs; rhs] } ->
      Printf.sprintf "%s != %s" (tptp_term lhs) (tptp_term rhs)
  | Neg a -> Printf.sprintf "~(%s)" (tptp_atom a)

let tptp_clause c =
  match canonicalize_clause_vars c with
  | [] -> "$false"
  | [lit] -> tptp_literal lit
  | lits -> String.concat " | " (List.map tptp_literal lits)

let tstp_clause_formula c =
  tptp_clause c

let tptp_rule_name rule =
  let b = Buffer.create (String.length rule) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> Buffer.add_char b (Char.lowercase_ascii c)
      | _ -> Buffer.add_char b '_')
    rule;
  let s = Buffer.contents b in
  if s = "" then "plain" else s

let tptp_clause_name id =
  if id < 0 then
    Printf.sprintf "ip_m%d" (-id)
  else
    Printf.sprintf "ip_%d" id

let tstp_derivation ?problem ?(prelude = "") ?(skip_initial = false) deriveds =
  let b = Buffer.create 4096 in
  begin
    match problem with
    | Some p -> Printf.bprintf b "%% SZS output start CNFRefutation for %s\n" p
    | None -> Printf.bprintf b "%% SZS output start CNFRefutation\n"
  end;
  Buffer.add_string b prelude;
  List.iter
    (fun d ->
      if not (skip_initial && d.parents = []) then begin
        let name = tptp_clause_name d.id in
        let role = if d.parents = [] then "axiom" else "plain" in
        let clause = tptp_clause d.clause_d in
        match d.parents with
        | [] ->
            Printf.bprintf b "cnf(%s,%s,(%s)).\n" name role clause
        | ps ->
            let parents =
              ps
              |> List.map tptp_clause_name
              |> String.concat ","
            in
            Printf.bprintf
              b
              "cnf(%s,%s,(%s),inference(%s,[status(thm)],[%s])).\n"
              name
              role
              clause
              (tptp_rule_name d.rule)
              parents
      end)
    deriveds;
  begin
    match problem with
    | Some p -> Printf.bprintf b "%% SZS output end CNFRefutation for %s\n" p
    | None -> Printf.bprintf b "%% SZS output end CNFRefutation\n"
  end;
  Buffer.contents b

let print_tstp_derivation ?problem ?prelude ?skip_initial deriveds =
  print_string (tstp_derivation ?problem ?prelude ?skip_initial deriveds)

let test_resolve mode c1 c2 =
  resolve_two_clauses ~check_timeout:(fun () -> ()) ~emulate_v1:false ~mode c1 c2

let test_factor mode c =
  factor_by_mode ~check_timeout:(fun () -> ()) ~emulate_v1:false mode c
