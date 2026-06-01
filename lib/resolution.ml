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

type derived = {
  id : int;
  parents : int list;
  rule : string;
  clause_d : clause;
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

let indexed_superpose_given ~check_timeout ~mode ~term_index ~all_by_id given =
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
                              ())
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
                              ())
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
        wall_clock_s = Unix.gettimeofday () -. start_t;
      };
    }
  in

  let active_clauses () =
    !active
  in

  let is_subsumed_by ds c =
    let rec aux = function
      | [] -> false
      | d :: tl ->
          check_timeout ();
          incr subsumption_tests;
          if subsumes d.clause_d c then true else aux tl
    in
    aux ds
  in

  let register_demodulator c =
    match oriented_demodulator_of_clause c with
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
        weight = clause_weight d.clause_d;
        d;
      }
    in
    incr next_passive_id;
    passive := entry :: !passive
  in

  let add_clause ~parents ~rule c =
    check_timeout ();
    if clause_limit_exceeded () then
      None
    else
      let c, rewrites =
        try rewrite_clause ~check_timeout !demodulators c
        with Rewrite_limit_hit -> (c, 0)
      in
      demodulation_rewrites := !demodulation_rewrites + rewrites;
      match simplify_clause c with
      | None -> None
      | Some c ->
          let key = string_of_clause c in
          if Hashtbl.mem known key then
            None
          else if is_subsumed_by (active_clauses ()) c then begin
            incr subsumption_rejections;
            None
          end else begin
            incr generated;
            let d = { id = next_id (); parents; rule; clause_d = c } in
            Hashtbl.add known key d.id;
            Hashtbl.replace all_by_id d.id d;
            all := d :: !all;
            enqueue_passive d;
            Some d
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
        if is_subsumed_by (active_clauses ()) c then begin
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

  let select_given () =
    match !passive with
    | [] -> None
    | entries ->
        let selected =
          (* Ratio 4:1 (Weight:Age) for given-clause selection *)
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
        if d.id <> given.id && subsumes given.clause_d d.clause_d then
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
        if e.d.id <> given.id && subsumes given.clause_d e.d.clause_d then
          delete_derived e.d
        else
          kept_passive := e :: !kept_passive)
      !passive;
    passive := List.rev !kept_passive
  in

  let activate given =
    backward_subsume given;
    active := given :: !active;
    register_demodulator given.clause_d;
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
          | Some d -> d :: acc
          | None -> acc)
        indexed_ids
        []
    in

    let fallback =
      List.fold_left
        (fun acc d ->
          check_timeout ();
          if d.id <> given.id && not (Hashtbl.mem indexed_ids d.id) then
            d :: acc
          else
            acc)
        []
        !active
    in

    indexed @ fallback
  in

  let add_generated ~parents ~rule c stop_reason =
    check_timeout ();
    if !stop_reason = None then
      match add_clause ~parents ~rule c with
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
