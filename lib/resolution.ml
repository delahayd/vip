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

(* -------------------------------------------------------------------------- *)
(* Lightweight AVATAR-style contexts and internal propositional solver.        *)
(*                                                                            *)
(* A first-order clause may be guarded by a propositional context.  The clause  *)
(* is usable only when all literals in its context are satisfied by the SAT     *)
(* assignment.  Splitting turns a first-order clause C1 \/ ... \/ Cn into a    *)
(* SAT clause p1 \/ ... \/ pn and n guarded first-order components Ci @ pi.    *)
(*                                                                            *)
(* This is intentionally small: unit propagation + chronological decisions.    *)
(* It is enough to make the saturation loop context-aware and to provide a      *)
(* clean place where CDCL features can later be added.                         *)
(* -------------------------------------------------------------------------- *)

type split_var = int

type prop_lit =
  | PPos of split_var
  | PNeg of split_var

type context = prop_lit list

type sat_result =
  | Sat_ok
  | Sat_conflict

module Sat_core = struct
  type assignment = True | False | Unassigned

  type t = {
    mutable next_var : int;
    clauses : prop_lit list list ref;
    assign : (split_var, assignment) Hashtbl.t;
    trail : prop_lit list ref;
  }

  let create () =
    { next_var = 0; clauses = ref []; assign = Hashtbl.create 127; trail = ref [] }

  let new_var st =
    st.next_var <- st.next_var + 1;
    Hashtbl.replace st.assign st.next_var Unassigned;
    st.next_var

  let neg = function
    | PPos v -> PNeg v
    | PNeg v -> PPos v

  let var_of_lit = function
    | PPos v | PNeg v -> v

  let bool_of_lit = function
    | PPos _ -> true
    | PNeg _ -> false

  let assignment_of_lit lit =
    if bool_of_lit lit then True else False

  let value_of_assignment = function
    | True -> Some true
    | False -> Some false
    | Unassigned -> None

  let eval_lit_in_array a lit =
    let v = var_of_lit lit in
    if v < 0 || v >= Array.length a then None
    else
      match value_of_assignment a.(v) with
      | None -> None
      | Some b -> Some (if bool_of_lit lit then b else not b)

  let eval_lit st lit =
    match Hashtbl.find_opt st.assign (var_of_lit lit) with
    | None | Some Unassigned -> None
    | Some True -> Some (bool_of_lit lit)
    | Some False -> Some (not (bool_of_lit lit))

  let add_clause st lits =
    st.clauses := lits :: !(st.clauses)

  let write_model st a =
    Hashtbl.clear st.assign;
    st.trail := [];
    for v = 1 to st.next_var do
      Hashtbl.replace st.assign v a.(v);
      match a.(v) with
      | True -> st.trail := PPos v :: !(st.trail)
      | False -> st.trail := PNeg v :: !(st.trail)
      | Unassigned -> ()
    done

  let propagate_array st a =
    let changed = ref true in
    let conflict = ref false in
    while !changed && not !conflict do
      changed := false;
      List.iter
        (fun clause ->
          if not !conflict then begin
            let satisfied = ref false in
            let unassigned = ref [] in
            List.iter
              (fun lit ->
                match eval_lit_in_array a lit with
                | Some true -> satisfied := true
                | Some false -> ()
                | None -> unassigned := lit :: !unassigned)
              clause;
            if not !satisfied then
              match !unassigned with
              | [] -> conflict := true
              | [ lit ] ->
                  let v = var_of_lit lit in
                  let wanted = assignment_of_lit lit in
                  begin
                    match wanted with
                    | Unassigned ->
                        ()
                    | True | False ->
                        begin
                          match a.(v), wanted with
                          | Unassigned, _ ->
                              a.(v) <- wanted;
                              changed := true
                          | True, True | False, False -> ()
                          | True, False | False, True -> conflict := true
                          | _, Unassigned -> assert false
                        end
                  end
              | _ -> ()
          end)
        !(st.clauses)
    done;
    if !conflict then Sat_conflict else Sat_ok

  let choose_unassigned st a =
    let rec aux v =
      if v > st.next_var then None
      else
        match a.(v) with
        | Unassigned -> Some v
        | True | False -> aux (v + 1)
    in
    aux 1

  let solve st =
    let rec dpll a =
      match propagate_array st a with
      | Sat_conflict -> None
      | Sat_ok ->
          begin match choose_unassigned st a with
          | None -> Some a
          | Some v ->
              let a_true = Array.copy a in
              a_true.(v) <- True;
              begin match dpll a_true with
              | Some model -> Some model
              | None ->
                  let a_false = Array.copy a in
                  a_false.(v) <- False;
                  dpll a_false
              end
          end
    in
    let a = Array.make (st.next_var + 1) Unassigned in
    match dpll a with
    | None -> Sat_conflict
    | Some model ->
        write_model st model;
        Sat_ok

  (* Backwards-compatible name.  The old implementation only propagated in the
     current trail and therefore treated a branch contradiction as a global SAT
     contradiction.  Here we deliberately re-solve the tiny propositional
     abstraction from scratch after each learned SAT clause, so failed branches
     simply force another Boolean model. *)


  let satisfiable_with_assumptions st assumptions =
    let a = Array.make (st.next_var + 1) Unassigned in
    let assumption_conflict = ref false in
    List.iter
      (fun lit ->
        if not !assumption_conflict then begin
          let v = var_of_lit lit in
          if v <= 0 || v >= Array.length a then
            assumption_conflict := true
          else begin
            let wanted = assignment_of_lit lit in
            match wanted with
            | Unassigned -> ()
            | True | False ->
                match a.(v), wanted with
                | Unassigned, _ -> a.(v) <- wanted
                | True, True | False, False -> ()
                | True, False | False, True -> assumption_conflict := true
                | _, Unassigned -> assert false
          end
        end)
      assumptions;
    if !assumption_conflict then
      false
    else
      let rec dpll a =
        match propagate_array st a with
        | Sat_conflict -> false
        | Sat_ok ->
            begin match choose_unassigned st a with
            | None -> true
            | Some v ->
                let a_true = Array.copy a in
                a_true.(v) <- True;
                if dpll a_true then true
                else begin
                  let a_false = Array.copy a in
                  a_false.(v) <- False;
                  dpll a_false
                end
            end
      in
      dpll a

  let context_sat st ctx =
    satisfiable_with_assumptions st ctx

  let propagate = solve

  let decide_first_unassigned st =
    let rec aux v =
      if v > st.next_var then None
      else
        match Hashtbl.find_opt st.assign v with
        | Some Unassigned | None -> Some (PPos v)
        | Some True | Some False -> aux (v + 1)
    in
    aux 1

  let assign_lit st lit =
    let v = var_of_lit lit in
    match Hashtbl.find_opt st.assign v with
    | Some True when bool_of_lit lit -> Sat_ok
    | Some False when not (bool_of_lit lit) -> Sat_ok
    | Some True | Some False -> Sat_conflict
    | None | Some Unassigned ->
        Hashtbl.replace st.assign v (assignment_of_lit lit);
        st.trail := lit :: !(st.trail);
        Sat_ok

  let context_satisfied st ctx =
    List.for_all
      (fun lit -> match eval_lit st lit with Some true -> true | _ -> false)
      ctx

  let context_consistent st ctx =
    List.for_all
      (fun lit -> match eval_lit st lit with Some false -> false | _ -> true)
      ctx

  let current_context st =
    Hashtbl.fold
      (fun v a acc ->
        match a with
        | True -> PPos v :: acc
        | False -> PNeg v :: acc
        | Unassigned -> acc)
      st.assign
      []
end

let string_of_prop_lit = function
  | PPos v -> Printf.sprintf "s%d" v
  | PNeg v -> Printf.sprintf "~s%d" v

let string_of_context = function
  | [] -> "{}"
  | ctx ->
      "{" ^ String.concat "," (List.map string_of_prop_lit ctx) ^ "}"

let normalize_context ctx =
  ctx
  |> List.sort_uniq compare

let context_key ctx =
  normalize_context ctx
  |> List.map string_of_prop_lit
  |> String.concat ";"

let context_subsumes c1 c2 =
  let c1 = normalize_context c1 in
  let c2 = normalize_context c2 in
  List.for_all (fun lit -> List.exists (( = ) lit) c2) c1

type derived = {
  id : int;
  parents : int list;
  rule : string;
  clause_d : clause;
  context : context;
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
  avatar_splits : int;
  avatar_split_components : int;
  avatar_split_rejected_disabled : int;
  avatar_split_rejected_short : int;
  avatar_split_rejected_equality : int;
  avatar_split_rejected_nonground : int;
  avatar_split_rejected_trivial : int;
  avatar_split_rejected_quota : int;
  avatar_context_clauses_generated : int;
  avatar_contextual_empty_conflicts : int;
  avatar_learned_conflicts : int;
  avatar_model_blocks : int;
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

let next_id () =
  incr id_counter;
  !id_counter

let option_exists f = function
  | Some x -> f x
  | None -> false

let atom_of_literal = function
  | Pos a | Neg a -> a

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

let selected_or_maximal_indices c =
  let negs = negative_indices c in
  if negs <> [] then negs else Ordering.maximal_literal_indices c

let is_active_literal mode c i =
  match mode with
  | Unrestricted -> true
  | Ordered | Ordered_with_fallback ->
      List.exists (( = ) i) (selected_or_maximal_indices c)

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
        if Hashtbl.mem seen key then
          aux acc rest
        else begin
          Hashtbl.add seen key ();
          aux ((c, parents) :: acc) rest
        end
  in
  aux [] items

let replace_nth xs n x =
  List.mapi (fun i y -> if i = n then x else y) xs

let replace_term_at_path t path replacement =
  let rec aux t path =
    match path, t with
    | [], _ -> replacement
    | i :: rest, Fun (f, args) ->
        let args' =
          List.mapi
            (fun j arg -> if i = j then aux arg rest else arg)
            args
        in
        Fun (f, args')
    | _ :: _, Var _ -> t
  in
  aux t path

let non_variable_subterms ~check_timeout t =
  let rec aux path acc = function
    | Var _ -> acc
    | Fun (_, args) as u ->
        check_timeout ();
        let acc = (path, u) :: acc in
        List.mapi (fun i x -> (i, x)) args
        |> List.fold_left
             (fun acc (i, child) ->
               check_timeout ();
               aux (path @ [ i ]) acc child)
             acc
  in
  aux [] [] t

let replace_atom_arg_at_path a arg_index path replacement =
  let args =
    List.mapi
      (fun i t ->
        if i = arg_index then replace_term_at_path t path replacement else t)
      a.args
  in
  { a with args }

let replace_literal_arg_at_path lit arg_index path replacement =
  match lit with
  | Pos a -> Pos (replace_atom_arg_at_path a arg_index path replacement)
  | Neg a -> Neg (replace_atom_arg_at_path a arg_index path replacement)

let oriented_sides mode l r =
  match mode with
  | Unrestricted ->
      begin
        match Ordering.orient_equation l r with
        | Some dir -> [ dir ]
        | None -> [ l, r; r, l ]
      end
  | Ordered ->
      begin
        match Ordering.orient_equation l r with
        | Some dir -> [ dir ]
        | None -> []
      end
  | Ordered_with_fallback ->
      begin
        match Ordering.orient_equation l r with
        | Some dir -> [ dir ]
        | None -> [ l, r; r, l ]
      end

let rec match_term env pattern target =
  match pattern with
  | Var v ->
      begin
        match StringMap.find_opt v env with
        | None -> Some (StringMap.add v target env)
        | Some t when t = target -> Some env
        | Some _ -> None
      end
  | Fun (f, ps) ->
      begin
        match target with
        | Var _ -> None
        | Fun (g, ts) ->
            if f <> g || List.length ps <> List.length ts then None
            else
              List.fold_left2
                (fun acc p t ->
                  match acc with
                  | None -> None
                  | Some env -> match_term env p t)
                (Some env)
                ps
                ts
      end

let rec rewrite_once_term ~check_timeout demods t =
  check_timeout ();
  let rec try_root = function
    | [] -> None
    | (lhs, rhs) :: rest ->
        check_timeout ();
        begin
          match match_term StringMap.empty lhs t with
          | Some env -> Some (apply_subst_term env rhs)
          | None -> try_root rest
        end
  in
  match try_root demods with
  | Some t' -> Some t'
  | None ->
      match t with
      | Var _ -> None
      | Fun (f, args) ->
          let rec rewrite_arg prefix = function
            | [] -> None
            | a :: suffix ->
                check_timeout ();
                begin
                  match rewrite_once_term ~check_timeout demods a with
                  | Some a' ->
                      Some (Fun (f, List.rev_append prefix (a' :: suffix)))
                  | None ->
                      rewrite_arg (a :: prefix) suffix
                end
          in
          rewrite_arg [] args

let rec rewrite_fix_term ~check_timeout demods count t =
  check_timeout ();
  if count > max_demodulation_steps_per_term then
    raise Rewrite_limit_hit
  else
    match rewrite_once_term ~check_timeout demods t with
    | None -> (t, count)
    | Some t' -> rewrite_fix_term ~check_timeout demods (count + 1) t'

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

let resolve_two_clauses ~check_timeout c1 c2 =
  let c1 = rename_clause_apart c1 in
  let c2 = rename_clause_apart c2 in
  let results = ref [] in
  List.iteri
    (fun i _ ->
      check_timeout ();
      List.iteri
        (fun j _ ->
          check_timeout ();
          match resolve_pair c1 c2 i j with
          | Some c -> results := c :: !results
          | None -> ())
        c2)
    c1;
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

let factor_pairs_ordered ~check_timeout c =
  let c = rename_clause_apart c in
  let active = selected_or_maximal_indices c in
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

let factor_by_mode ~check_timeout mode c =
  match mode with
  | Unrestricted -> factor_pairs_unrestricted ~check_timeout c
  | Ordered -> factor_pairs_ordered ~check_timeout c
  | Ordered_with_fallback ->
      let ordered = factor_pairs_ordered ~check_timeout c in
      if ordered <> [] then ordered
      else factor_pairs_unrestricted ~check_timeout c

let equality_resolution ~check_timeout mode c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iteri
    (fun i lit ->
      check_timeout ();
      if is_active_literal mode c i then
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

let equality_factoring ~check_timeout mode c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iteri
    (fun i lit1 ->
      check_timeout ();
      if is_active_literal mode c i then
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

let indexed_superpose_given ~check_timeout ~mode ~term_index ~all_by_id ~compatible given =
  let results = ref [] in

  (* Direction 1 :
     égalités du given -> sous-termes des clauses indexées *)
  let given_source_clause = rename_clause_apart given.clause_d in

  List.iteri
    (fun eq_index lit ->
      check_timeout ();

      if is_active_literal mode given_source_clause eq_index then
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
                          if compatible given target_d then (
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
                                ()))
                  entries)
              (oriented_sides mode l r))
    given_source_clause;

  (* Direction 2 :
     égalités indexées -> sous-termes du given *)
  let given_target_clause = rename_clause_apart given.clause_d in

  List.iteri
    (fun lit_index lit ->
      check_timeout ();

      if is_active_literal mode given_target_clause lit_index then
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
                          if compatible eq_d given then (
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
                                ()))
                  eqs)
              subterms)
          a.args)
    given_target_clause;

  dedup_clauses_with_parents ~check_timeout (List.rev !results)

let term_index_orientation_mode = function
  | Ordered -> Term_index.Oriented_only
  | Unrestricted | Ordered_with_fallback -> Term_index.Both_if_unorientable

let run_resolution_sos ?(limits = default_limits) ~mode ~axioms ~support () =
  let start_t = Unix.gettimeofday () in

  let known : (string, int) Hashtbl.t = Hashtbl.create 4099 in
  let all_by_id : (int, derived) Hashtbl.t = Hashtbl.create 4099 in
  let active : derived list ref = ref [] in
  let passive : passive_entry list ref = ref [] in
  let all : derived list ref = ref [] in
  let demodulators : (term * term) list ref = ref [] in

  (* Indexes contain only active clauses.  Backward simplification may delete
     active clauses; stale index hits are filtered through [all_by_id]. *)
  let literal_index = Discrimination_index.create () in
  let term_index = Term_index.create () in
  let sat = Sat_core.create () in
  let sat_unsat = ref false in
  let avatar_models_exhausted = ref false in

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

  let avatar_split_attempts = ref 0 in
  let avatar_successful_splits = ref 0 in
  let avatar_split_components = ref 0 in
  let avatar_split_rejected_disabled = ref 0 in
  let avatar_split_rejected_short = ref 0 in
  let avatar_split_rejected_equality = ref 0 in
  let avatar_split_rejected_nonground = ref 0 in
  let avatar_split_rejected_trivial = ref 0 in
  let avatar_split_rejected_quota = ref 0 in
  let avatar_context_clauses_generated = ref 0 in
  let avatar_contextual_empty_conflicts = ref 0 in
  let avatar_model_blocks = ref 0 in
  let avatar_sat_clauses_added = ref 0 in
  let avatar_sat_solves = ref 0 in
  let avatar_sat_conflicts = ref 0 in
  let avatar_context_sat_tests = ref 0 in
  let avatar_context_sat_failures = ref 0 in
  let avatar_filtered_inferences = ref 0 in

  let sat_add_clause lits =
    incr avatar_sat_clauses_added;
    Sat_core.add_clause sat lits
  in

  let sat_context_sat ctx =
    incr avatar_context_sat_tests;
    let ok = Sat_core.context_sat sat ctx in
    if not ok then incr avatar_context_sat_failures;
    ok
  in

  let sat_solve () =
    incr avatar_sat_solves;
    match Sat_core.propagate sat with
    | Sat_ok -> Sat_ok
    | Sat_conflict ->
        incr avatar_sat_conflicts;
        Sat_conflict
  in

  let sat_propagate_or_mark_conflict () =
    match sat_solve () with
    | Sat_ok -> ()
    | Sat_conflict -> sat_unsat := true
  in

  let sat_propagate_or_mark_exhausted () =
    match sat_solve () with
    | Sat_ok -> ()
    | Sat_conflict -> avatar_models_exhausted := true
  in

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

  let clause_weight c =
    let base =
      List.fold_left (fun acc lit -> acc + literal_weight lit) 0 c
    in
    let unit_bonus =
      if List.length c = 1 then -2 else 0
    in
    let negative_bonus =
      if clause_has_negative c then -1 else 0
    in
    let equality_penalty =
      if clause_has_equality c then 2 else 0
    in
    max 1 (base + unit_bonus + negative_bonus + equality_penalty)
  in

  let concat_map f xs =
    List.fold_right (fun x acc -> f x @ acc) xs []
  in

  let rec vars_of_term = function
    | Var v -> [ v ]
    | Fun (_, args) ->
        concat_map vars_of_term args
        |> List.sort_uniq String.compare
  in

  let vars_of_literal lit =
    let a = atom_of_literal lit in
    concat_map vars_of_term a.args
    |> List.sort_uniq String.compare
  in

  let disjoint xs ys =
    not (List.exists (fun x -> List.exists (( = ) x) ys) xs)
  in

  let split_components c =
    (* Conservative splitting: merge literals whose variable sets overlap.
       Ground literals are mutually independent and therefore split apart. *)
    let rec add_component lit vars = function
      | [] -> [ ([ lit ], vars) ]
      | (lits, vs) :: rest ->
          if disjoint vars vs then
            (lits, vs) :: add_component lit vars rest
          else
            (lit :: lits, List.sort_uniq String.compare (vars @ vs)) :: rest
    in
    let comps =
      List.fold_left
        (fun acc lit -> add_component lit (vars_of_literal lit) acc)
        []
        c
    in
    comps
    |> List.map (fun (lits, _) -> List.rev lits)
    |> List.rev
  in

  let getenv_bool name default =
    match Sys.getenv_opt name with
    | None -> default
    | Some "" -> default
    | Some "0" | Some "false" | Some "False" | Some "no" | Some "NO" -> false
    | Some _ -> true
  in

  let getenv_int name default =
    match Sys.getenv_opt name with
    | None -> default
    | Some s ->
        (try max 0 (int_of_string s) with Failure _ -> default)
  in

  (* Conservative AVATAR parameters.

     The key point is that splitting must not replace the ordinary saturation
     problem too aggressively.  In this first robust variant we split only
     clauses that are very unlikely to be essential short first-order links:

       - ground clauses only;
       - no equality;
       - at least [avatar_min_split_literals] literals;
       - global quota on Boolean split variables;
       - the original clause is kept as well.

     Environment knobs are intentionally provided for benchmarking:

       IP_AVATAR_SPLITTING=0        disables splitting, keeps contexts/SAT code
       IP_AVATAR_MIN_SPLIT=4        minimum clause length to split
       IP_AVATAR_MAX_SPLIT_VARS=64  global Boolean variable quota
       IP_AVATAR_KEEP_ORIGINAL=1    keep the unsplit clause too
  *)
  let avatar_splitting_enabled =
    getenv_bool "IP_AVATAR_SPLITTING" true
  in

  let avatar_keep_original_clause =
    getenv_bool "IP_AVATAR_KEEP_ORIGINAL" true
  in

  let avatar_min_split_literals =
    getenv_int "IP_AVATAR_MIN_SPLIT" 4
  in

  let avatar_max_split_vars =
    getenv_int "IP_AVATAR_MAX_SPLIT_VARS" 64
  in

  let avatar_split_vars_used = ref 0 in

  let literal_is_ground lit =
    vars_of_literal lit = []
  in

  let clause_is_ground c =
    List.for_all literal_is_ground c
  in

  let should_split c =
    incr avatar_split_attempts;
    if not avatar_splitting_enabled then begin
      incr avatar_split_rejected_disabled;
      None
    end else if List.length c < avatar_min_split_literals then begin
      incr avatar_split_rejected_short;
      None
    end else if clause_has_equality c then begin
      incr avatar_split_rejected_equality;
      None
    end else if not (clause_is_ground c) then begin
      incr avatar_split_rejected_nonground;
      None
    end else
      match split_components c with
      | [] | [ _ ] ->
          incr avatar_split_rejected_trivial;
          None
      | comps ->
          let n = List.length comps in
          if !avatar_split_vars_used + n > avatar_max_split_vars then begin
            incr avatar_split_rejected_quota;
            None
          end else
            Some comps
  in

  let first_order_compatible d1 d2 =
    (* Important: compatibility must be propositional satisfiability of the
       union of the two guards, not truth in the current SAT model.  Otherwise
       two clauses activated under different models may never be combined, even
       though a later model can make both guards true.

       A negative answer means that the corresponding binary inference is
       filtered by AVATAR. *)
    let ok = sat_context_sat (d1.context @ d2.context) in
    if not ok then incr avatar_filtered_inferences;
    ok
  in

  let active_under_current_sat d =
    Sat_core.context_satisfied sat d.context
  in

  let visible_under_current_sat d =
    Sat_core.context_consistent sat d.context
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

        avatar_enabled = avatar_splitting_enabled;
        avatar_keep_original = avatar_keep_original_clause;
        avatar_min_split_literals = avatar_min_split_literals;
        avatar_max_split_vars = avatar_max_split_vars;
        avatar_split_vars_used = !avatar_split_vars_used;
        avatar_split_attempts = !avatar_split_attempts;
        avatar_successful_splits = !avatar_successful_splits;
        avatar_splits = !avatar_successful_splits;
        avatar_split_components = !avatar_split_components;
        avatar_split_rejected_disabled = !avatar_split_rejected_disabled;
        avatar_split_rejected_short = !avatar_split_rejected_short;
        avatar_split_rejected_equality = !avatar_split_rejected_equality;
        avatar_split_rejected_nonground = !avatar_split_rejected_nonground;
        avatar_split_rejected_trivial = !avatar_split_rejected_trivial;
        avatar_split_rejected_quota = !avatar_split_rejected_quota;
        avatar_context_clauses_generated = !avatar_context_clauses_generated;
        avatar_contextual_empty_conflicts = !avatar_contextual_empty_conflicts;
        avatar_learned_conflicts = !avatar_contextual_empty_conflicts;
        avatar_model_blocks = !avatar_model_blocks;
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

  let passive_clauses () =
    List.map (fun e -> e.d) !passive
  in

  let existing_clauses () =
    active_clauses () @ passive_clauses ()
  in

  let is_subsumed_by ds context c =
    let rec aux = function
      | [] -> false
      | d :: tl ->
          check_timeout ();
          incr subsumption_tests;
          if context_subsumes d.context context && subsumes d.clause_d c then
            true
          else
            aux tl
    in
    aux ds
  in

  let register_demodulator d =
    if d.context <> [] then ()
    else match oriented_demodulator_of_clause d.clause_d with
    | None -> ()
    | Some rule ->
        if not (List.exists (( = ) rule) !demodulators) then
          demodulators := rule :: !demodulators
  in

  let index_active_clause d =
    Discrimination_index.add_clause literal_index ~clause_id:d.id d.clause_d;
    Term_index.add_clause
      term_index
      ~clause_id:d.id
      ~orientation_mode:(term_index_orientation_mode mode)
      d.clause_d
  in

  let enqueue_passive d =
    let entry =
      {
        passive_id = !next_passive_id;
        age = !next_passive_id;
        (* Contextual AVATAR components are useful, but they should not
           systematically preempt the old unsplit search.  A small penalty
           lets ordinary root clauses keep roughly their previous priority. *)
        weight = clause_weight d.clause_d + (if d.context = [] then 0 else 8);
        d;
      }
    in
    incr next_passive_id;
    passive := entry :: !passive
  in

  let rec add_clause ?(context = []) ?(allow_split = true) ~parents ~rule c =
    check_timeout ();
    let context = normalize_context context in
    if !sat_unsat || not (sat_context_sat context) then
      None
    else if clause_limit_exceeded () then
      None
    else
      let c, rewrites =
        try rewrite_clause ~check_timeout !demodulators c
        with Rewrite_limit_hit -> (c, 0)
      in
      demodulation_rewrites := !demodulation_rewrites + rewrites;
      match simplify_clause c with
      | None -> None
      | Some [] ->
          if context = [] then begin
            incr generated;
            let d = { id = next_id (); parents; rule; clause_d = []; context } in
            all := d :: !all;
            Hashtbl.replace all_by_id d.id d;
            Some d
          end else begin
            (* A first-order contradiction under context Γ learns the SAT
               clause not Γ.  If that makes SAT inconsistent, the whole
               problem is refuted propositionally. *)
            incr avatar_contextual_empty_conflicts;
            sat_add_clause (List.map Sat_core.neg context);
            sat_propagate_or_mark_conflict ();
            None
          end
      | Some c ->
          begin
            match if allow_split then should_split c else None with
            | Some comps ->
                (* Conservative splitting: keep the original clause.

                   This preserves the old given-clause proof search path.  The
                   split components are an additional AVATAR-guided search
                   layer, not a replacement for the original clause. *)
                let original_result =
                  if avatar_keep_original_clause then
                    add_clause
                      ~context
                      ~allow_split:false
                      ~parents
                      ~rule:(rule ^ "/avatar_original")
                      c
                  else
                    None
                in
                incr avatar_successful_splits;
                avatar_split_components := !avatar_split_components + List.length comps;
                let vars = List.map (fun _ -> Sat_core.new_var sat) comps in
                avatar_split_vars_used := !avatar_split_vars_used + List.length vars;
                let plits = List.map (fun v -> PPos v) vars in
                sat_add_clause (List.map Sat_core.neg context @ plits);
                sat_propagate_or_mark_conflict ();
                List.iter2
                  (fun p comp ->
                    ignore
                      (add_clause
                         ~context:(p :: context)
                         ~allow_split:false
                         ~parents
                         ~rule:(rule ^ "/split_component")
                         comp))
                  plits
                  comps;
                original_result
            | None ->
                let key = context_key context ^ "|" ^ string_of_clause c in
                if Hashtbl.mem known key then
                  None
                else if is_subsumed_by (existing_clauses ()) context c then begin
                  incr subsumption_rejections;
                  None
                end else begin
                  incr generated;
                  let d = { id = next_id (); parents; rule; clause_d = c; context } in
                  Hashtbl.add known key d.id;
                  Hashtbl.replace all_by_id d.id d;
                  all := d :: !all;
                  enqueue_passive d;
                  Some d
                end
          end
  in

  let simplify_selected d =
    check_timeout ();
    let c, rewrites =
      try rewrite_clause ~check_timeout !demodulators d.clause_d
      with Rewrite_limit_hit -> (d.clause_d, 0)
    in
    demodulation_rewrites := !demodulation_rewrites + rewrites;
    match simplify_clause c with
    | None -> None
    | Some c ->
        if is_subsumed_by (active_clauses ()) d.context c then begin
          incr subsumption_rejections;
          None
        end else
          let d' = { d with clause_d = c } in
          Hashtbl.replace all_by_id d'.id d';
          Some d'
  in

  let select_by_age entries =
    List.fold_left
      (fun best e ->
        match best with
        | None -> Some e
        | Some b -> if e.age < b.age then Some e else best)
      None
      entries
  in

  let select_by_weight entries =
    List.fold_left
      (fun best e ->
        match best with
        | None -> Some e
        | Some b ->
            if e.weight < b.weight
               || (e.weight = b.weight && e.age < b.age)
            then Some e
            else best)
      None
      entries
  in

  let remove_passive_entry selected =
    passive :=
      List.filter
        (fun e -> e.passive_id <> selected.passive_id)
        !passive
  in

  let rec select_given () =
    sat_propagate_or_mark_conflict ();
    if !sat_unsat || !avatar_models_exhausted then
      None
    else
      let entries =
        !passive
        |> List.filter (fun e -> visible_under_current_sat e.d)
        |> List.filter (fun e -> active_under_current_sat e.d)
      in
      match entries with
      | [] ->
          (* The current Boolean abstraction model is saturated: there is no
             currently active first-order clause left to process.  This is not
             a refutation.  To keep the AVATAR search complete enough for small
             propositional/splitting problems, block this model and ask the
             internal SAT solver for another one.  If all Boolean models are
             exhausted by these exploration blockers, we report Saturation/GaveUp,
             not Unsatisfiable.  Only clauses learned from first-order empty
             clauses may set [sat_unsat]. *)
          let model = Sat_core.current_context sat |> normalize_context in
          incr avatar_model_blocks;
          sat_add_clause (List.map Sat_core.neg model);
          sat_propagate_or_mark_exhausted ();
          if !avatar_models_exhausted then None else select_given ()
      | entries ->
          let selected =
            (* Four weight selections, then one age selection. *)
            if !given_count mod 5 = 4 then
              select_by_age entries
            else
              select_by_weight entries
          in
          match selected with
          | None -> None
          | Some e ->
              remove_passive_entry e;
              incr given_count;
              Some e.d
  in

  let delete_derived d =
    Hashtbl.remove all_by_id d.id
  in

  let backward_subsume given =
    let kept_active = ref [] in
    List.iter
      (fun d ->
        check_timeout ();
        incr subsumption_tests;
        if d.id <> given.id
           && context_subsumes given.context d.context
           && subsumes given.clause_d d.clause_d then
          delete_derived d
        else
          kept_active := d :: !kept_active)
      !active;
    active := List.rev !kept_active;

    let kept_passive = ref [] in
    List.iter
      (fun e ->
        check_timeout ();
        incr subsumption_tests;
        if e.d.id <> given.id
           && context_subsumes given.context e.d.context
           && subsumes given.clause_d e.d.clause_d then
          delete_derived e.d
        else
          kept_passive := e :: !kept_passive)
      !passive;
    passive := List.rev !kept_passive
  in

  let activate given =
    backward_subsume given;
    active := given :: !active;
    register_demodulator given;
    index_active_clause given
  in

  let resolution_candidates given =
    let indexed_ids = Hashtbl.create 127 in

    List.iter
      (fun lit ->
        check_timeout ();
        let entries =
          Discrimination_index.find_complementary literal_index lit
        in
        List.iter
          (fun e ->
            check_timeout ();
            if e.Discrimination_index.clause_id <> given.id then
              Hashtbl.replace indexed_ids e.Discrimination_index.clause_id ())
          entries)
      given.clause_d;

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
          if d.id <> given.id
             && not (Hashtbl.mem indexed_ids d.id)
             && first_order_compatible given d then
            d :: acc
          else
            acc)
        []
        !active
    in

    indexed @ fallback
  in


  let context_of_parent_ids ids =
    ids
    |> List.fold_left
         (fun acc id ->
           match Hashtbl.find_opt all_by_id id with
           | None -> acc
           | Some d -> d.context @ acc)
         []
    |> normalize_context
  in

  let add_generated ?(context = []) ~parents ~rule c stop_reason =
    check_timeout ();
    if !stop_reason = None then
      match add_clause ~context ~parents ~rule c with
      | Some d when d.clause_d = [] ->
          stop_reason := Some (Refutation_found d)
      | _ -> ()
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

    (* This is intentionally not a strict set-of-support loop: both axioms and
       support clauses start passive, and the age/weight selector decides the
       next given clause.  Support/conjecture clauses can still be biased later
       by adding metadata and changing [clause_weight]. *)
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

      if clause_limit_exceeded () then
        stop_reason := Some Clause_limit
      else
        match select_given () with
        | None ->
            if !sat_unsat then
              stop_reason := Some (Refutation_found { id = next_id (); parents = []; rule = "avatar_sat_conflict"; clause_d = []; context = [] })
            else
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
                    (* The given clause becomes active before inference.  This
                       lets the term/literal indexes represent Active.  Stale
                       self-hits are filtered by ids in the inference code. *)
                    activate given;

                    List.iter
                      (fun fc ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr factoring_inferences;
                          add_generated
                            ~context:given.context
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
                      (factor_by_mode ~check_timeout mode given.clause_d);

                    List.iter
                      (fun c ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr equality_resolution_inferences;
                          add_generated
                            ~context:given.context
                            ~parents:[ given.id ]
                            ~rule:"equality_resolution"
                            c
                            stop_reason
                        end)
                      (equality_resolution ~check_timeout mode given.clause_d);

                    List.iter
                      (fun c ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr equality_factoring_inferences;
                          add_generated
                            ~context:given.context
                            ~parents:[ given.id ]
                            ~rule:"equality_factoring"
                            c
                            stop_reason
                        end)
                      (equality_factoring ~check_timeout mode given.clause_d);

                    List.iter
                      (fun other ->
                        check_timeout ();
                        if !stop_reason = None && other.id <> given.id then begin
                          let resolvents =
                            resolve_two_clauses
                              ~check_timeout
                              given.clause_d
                              other.clause_d
                          in
                          List.iter
                            (fun r ->
                              check_timeout ();
                              if !stop_reason = None then begin
                                incr resolution_inferences;
                                add_generated
                                  ~context:(given.context @ other.context)
                                  ~parents:[ given.id; other.id ]
                                  ~rule:"resolution"
                                  r
                                  stop_reason
                              end)
                            resolvents
                        end)
                      (resolution_candidates given);

                    List.iter
                      (fun (r, parents) ->
                        check_timeout ();
                        if !stop_reason = None then begin
                          incr superposition_inferences;
                          add_generated
                            ~context:(context_of_parent_ids parents)
                            ~parents
                            ~rule:"indexed_superposition"
                            r
                            stop_reason
                        end)
                      (indexed_superpose_given
                         ~check_timeout
                         ~mode
                         ~term_index
                         ~all_by_id
                         ~compatible:first_order_compatible
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
        "%4d. %-30s [%s] %-12s %s\n"
        d.id
        d.rule
        parents
        (string_of_context d.context)
        (string_of_clause d.clause_d))
    deriveds
