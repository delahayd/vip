open Types
open Subst
open Unif
open Clause
open Pretty

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

let dedup_clauses cls =
  let sorted = List.sort compare cls in
  let rec aux acc = function
    | a :: (b :: _ as tl) when a = b -> aux acc tl
    | a :: tl -> aux (a :: acc) tl
    | [] -> List.rev acc
  in
  aux [] sorted

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

let non_variable_subterms t =
  let rec aux path acc = function
    | Var _ -> acc
    | Fun (_, args) as t ->
        let acc = (path, t) :: acc in
        List.mapi (fun i x -> (i, x)) args
        |> List.fold_left
             (fun acc (i, child) -> aux (path @ [ i ]) acc child)
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
  let c = Ordering.compare_term_kbo l r in
  match mode with
  | Unrestricted ->
      if c > 0 then [ (l, r) ]
      else if c < 0 then [ (r, l) ]
      else [ (l, r); (r, l) ]
  | Ordered ->
      if c > 0 then [ (l, r) ]
      else if c < 0 then [ (r, l) ]
      else []
  | Ordered_with_fallback ->
      if c > 0 then [ (l, r) ]
      else if c < 0 then [ (r, l) ]
      else [ (l, r); (r, l) ]

(* Matching pour demodulation : pattern -> target *)

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

let rec rewrite_once_term demods t =
  let rec try_root = function
    | [] -> None
    | (lhs, rhs) :: rest ->
        begin
          match match_term StringMap.empty lhs t with
          | Some env -> Some (apply_subst_term env rhs)
          | None -> try_root rest
        end
  in
  match try_root demods with
  | Some t' -> Some t'
  | None ->
      begin
        match t with
        | Var _ -> None
        | Fun (f, args) ->
            let rec rewrite_arg prefix = function
              | [] -> None
              | a :: suffix ->
                  begin
                    match rewrite_once_term demods a with
                    | Some a' ->
                        Some (Fun (f, List.rev_append prefix (a' :: suffix)))
                    | None ->
                        rewrite_arg (a :: prefix) suffix
                  end
            in
            rewrite_arg [] args
      end

let rec rewrite_fix_term demods count t =
  match rewrite_once_term demods t with
  | None -> (t, count)
  | Some t' -> rewrite_fix_term demods (count + 1) t'

let rewrite_atom demods count a =
  let args, count =
    List.fold_left
      (fun (acc, count) t ->
        let t', count = rewrite_fix_term demods count t in
        (t' :: acc, count))
      ([], count)
      a.args
  in
  ({ a with args = List.rev args }, count)

let rewrite_literal demods count = function
  | Pos a ->
      let a', count = rewrite_atom demods count a in
      (Pos a', count)
  | Neg a ->
      let a', count = rewrite_atom demods count a in
      (Neg a', count)

let rewrite_clause demods c =
  List.fold_left
    (fun (acc, count) lit ->
      let lit', count = rewrite_literal demods count lit in
      (lit' :: acc, count))
    ([], 0)
    c
  |> fun (lits, count) -> (List.rev lits, count)

let oriented_demodulator_of_clause = function
  | [ Pos { pred = "="; args = [ l; r ] } ] ->
      if Ordering.compare_term_kbo l r > 0 then Some (l, r)
      else if Ordering.compare_term_kbo r l > 0 then Some (r, l)
      else None
  | _ -> None

(* Résolution classique *)

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
      with Not_unifiable -> None
    else None
  else None

let resolve_unrestricted c1 c2 =
  let c1 = rename_clause_apart c1 in
  let c2 = rename_clause_apart c2 in
  let results = ref [] in
  List.iter
    (fun i ->
      List.iter
        (fun j ->
          match resolve_pair c1 c2 i j with
          | Some c -> results := c :: !results
          | None -> ())
        (all_indices c2))
    (all_indices c1);
  dedup_clauses !results

let resolve_ordered_strict c1 c2 =
  let c1 = rename_clause_apart c1 in
  let c2 = rename_clause_apart c2 in
  let active1 = selected_or_maximal_indices c1 in
  let active2 = selected_or_maximal_indices c2 in
  let results = ref [] in
  List.iter
    (fun i ->
      List.iter
        (fun j ->
          match resolve_pair c1 c2 i j with
          | Some c -> results := c :: !results
          | None -> ())
        active2)
    active1;
  dedup_clauses !results

let resolve_by_mode mode c1 c2 =
  match mode with
  | Unrestricted -> resolve_unrestricted c1 c2
  | Ordered -> resolve_ordered_strict c1 c2
  | Ordered_with_fallback ->
      let ordered = resolve_ordered_strict c1 c2 in
      if ordered <> [] then ordered else resolve_unrestricted c1 c2

(* Factoring classique *)

let factor_pairs_unrestricted c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iter
    (fun i ->
      List.iter
        (fun j ->
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
  dedup_clauses !results

let factor_pairs_ordered c =
  let c = rename_clause_apart c in
  let active = selected_or_maximal_indices c in
  let results = ref [] in
  List.iter
    (fun i ->
      List.iter
        (fun j ->
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
  dedup_clauses !results

let factor_by_mode mode c =
  match mode with
  | Unrestricted -> factor_pairs_unrestricted c
  | Ordered -> factor_pairs_ordered c
  | Ordered_with_fallback ->
      let ordered = factor_pairs_ordered c in
      if ordered <> [] then ordered else factor_pairs_unrestricted c

(* Règles égalitaires *)

let equality_resolution mode c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iteri
    (fun i lit ->
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
  dedup_clauses !results

let equality_factoring mode c =
  let c = rename_clause_apart c in
  let results = ref [] in
  List.iteri
    (fun i lit1 ->
      if is_active_literal mode c i then
        match positive_equality lit1 with
        | None -> ()
        | Some (l1, r1) ->
            List.iteri
              (fun j lit2 ->
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
  dedup_clauses !results

let superpose_from_into mode source target =
  let source = rename_clause_apart source in
  let target = rename_clause_apart target in
  let results = ref [] in

  List.iteri
    (fun eq_index eq_lit ->
      if is_active_literal mode source eq_index then
        match positive_equality eq_lit with
        | None -> ()
        | Some (l, r) ->
            let dirs = oriented_sides mode l r in
            List.iter
              (fun (lhs, rhs) ->
                let source_rest =
                  List.filteri (fun i _ -> i <> eq_index) source
                in
                List.iteri
                  (fun lit_index lit ->
                    if is_active_literal mode target lit_index then
                      let a = atom_of_literal lit in
                      List.iteri
                        (fun arg_index arg ->
                          non_variable_subterms arg
                          |> List.iter
                               (fun (path, subterm) ->
                                 try
                                   let s = unify_terms lhs subterm empty_subst in
                                   let rhs' = apply_subst_term s rhs in
                                   let lit' =
                                     replace_literal_arg_at_path
                                       lit arg_index path rhs'
                                   in
                                   let target' =
                                     List.mapi
                                       (fun i x ->
                                         if i = lit_index then lit' else x)
                                       target
                                   in
                                   let combined =
                                     apply_subst_clause s (source_rest @ target')
                                   in
                                   match simplify_clause combined with
                                   | Some c -> results := c :: !results
                                   | None -> ()
                                 with Not_unifiable -> ()))
                        a.args)
                  target)
              dirs)
    source;

  dedup_clauses !results

let superpose_between mode c1 c2 =
  dedup_clauses
    (superpose_from_into mode c1 c2 @ superpose_from_into mode c2 c1)

(* Boucle principale *)

let run_resolution_sos ?(limits = default_limits) ~mode ~axioms ~support () =
  let start_t = Unix.gettimeofday () in

  let known : (string, int) Hashtbl.t = Hashtbl.create 4099 in
  let all_by_id : (int, derived) Hashtbl.t = Hashtbl.create 4099 in
  let agenda : derived list ref = ref [] in
  let all : derived list ref = ref [] in
  let demodulators : (term * term) list ref = ref [] in

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

  let time_exceeded () =
    option_exists
      (fun t -> Unix.gettimeofday () -. start_t >= t)
      limits.time_limit_s
  in

  let clause_limit_exceeded () =
    option_exists
      (fun n -> !generated >= n)
      limits.max_generated_clauses
  in

  let clause_has_negative c =
    List.exists is_negative c
  in

  let better_clause a b =
    let la = List.length a.clause_d in
    let lb = List.length b.clause_d in
    if la <> lb then la < lb
    else
      let na = clause_has_negative a.clause_d in
      let nb = clause_has_negative b.clause_d in
      if na <> nb then na else a.id < b.id
  in

  let rec insert_agenda d = function
    | [] -> [ d ]
    | x :: xs as l ->
        if better_clause d x then d :: l
        else x :: insert_agenda d xs
  in

  let enqueue d =
    agenda := insert_agenda d !agenda
  in

  let pop_agenda () =
    match !agenda with
    | [] -> None
    | x :: xs ->
        agenda := xs;
        Some x
  in

  let existing_clauses () =
    Hashtbl.fold (fun _ d acc -> d :: acc) all_by_id []
  in

  let is_subsumed_by_existing c =
    let rec aux = function
      | [] -> false
      | d :: tl ->
          incr subsumption_tests;
          if subsumes d.clause_d c then true else aux tl
    in
    aux (existing_clauses ())
  in

  let register_demodulator c =
    match oriented_demodulator_of_clause c with
    | None -> ()
    | Some rule ->
        if not (List.exists (( = ) rule) !demodulators) then
          demodulators := rule :: !demodulators
  in

  let add_clause ~to_support ~parents ~rule c =
    let c, rewrites = rewrite_clause !demodulators c in
    demodulation_rewrites := !demodulation_rewrites + rewrites;
    match simplify_clause c with
    | None -> None
    | Some c ->
        let key = string_of_clause c in
        if Hashtbl.mem known key then
          None
        else if is_subsumed_by_existing c then begin
          incr subsumption_rejections;
          None
        end else begin
          incr generated;
          let d = {
            id = next_id ();
            parents;
            rule;
            clause_d = c;
          } in
          Hashtbl.add known key d.id;
          Hashtbl.replace all_by_id d.id d;
          all := d :: !all;
          register_demodulator c;
          if to_support then enqueue d;
          Some d
        end
  in

  let stop_reason = ref None in

  let add_initial ~to_support c =
    match add_clause ~to_support ~parents:[] ~rule:"input" c with
    | Some d when d.clause_d = [] ->
        stop_reason := Some (Refutation_found d)
    | _ -> ()
  in

  List.iter
    (fun c -> if !stop_reason = None then add_initial ~to_support:false c)
    axioms;

  List.iter
    (fun c -> if !stop_reason = None then add_initial ~to_support:true c)
    support;

  while !stop_reason = None do
    if time_exceeded () then
      stop_reason := Some Time_limit
    else if clause_limit_exceeded () then
      stop_reason := Some Clause_limit
    else
      match pop_agenda () with
      | None ->
          stop_reason := Some Saturation
      | Some given ->
          if Hashtbl.mem all_by_id given.id then begin
            incr processed;

            let factors = factor_by_mode mode given.clause_d in
            List.iter
              (fun fc ->
                if !stop_reason = None then begin
                  incr factoring_inferences;
                  match
                    add_clause
                      ~to_support:true
                      ~parents:[ given.id ]
                      ~rule:
                        (match mode with
                         | Unrestricted -> "factor"
                         | Ordered -> "ordered_factor"
                         | Ordered_with_fallback -> "ordered_factor/fallback")
                      fc
                  with
                  | Some d when d.clause_d = [] ->
                      stop_reason := Some (Refutation_found d)
                  | _ -> ()
                end)
              factors;

            let eq_res = equality_resolution mode given.clause_d in
            List.iter
              (fun c ->
                if !stop_reason = None then begin
                  incr equality_resolution_inferences;
                  match
                    add_clause
                      ~to_support:true
                      ~parents:[ given.id ]
                      ~rule:"equality_resolution"
                      c
                  with
                  | Some d when d.clause_d = [] ->
                      stop_reason := Some (Refutation_found d)
                  | _ -> ()
                end)
              eq_res;

            let eq_fact = equality_factoring mode given.clause_d in
            List.iter
              (fun c ->
                if !stop_reason = None then begin
                  incr equality_factoring_inferences;
                  match
                    add_clause
                      ~to_support:true
                      ~parents:[ given.id ]
                      ~rule:"equality_factoring"
                      c
                  with
                  | Some d when d.clause_d = [] ->
                      stop_reason := Some (Refutation_found d)
                  | _ -> ()
                end)
              eq_fact;

            let candidates =
              existing_clauses ()
              |> List.filter (fun d -> d.id <> given.id)
            in

            List.iter
              (fun other ->
                if !stop_reason = None && Hashtbl.mem all_by_id other.id then begin
                  if time_exceeded () then
                    stop_reason := Some Time_limit
                  else if clause_limit_exceeded () then
                    stop_reason := Some Clause_limit
                  else begin
                    let resolvents =
                      resolve_by_mode mode given.clause_d other.clause_d
                    in
                    List.iter
                      (fun r ->
                        if !stop_reason = None then begin
                          incr resolution_inferences;
                          match
                            add_clause
                              ~to_support:true
                              ~parents:[ given.id; other.id ]
                              ~rule:
                                (match mode with
                                 | Unrestricted -> "resolution"
                                 | Ordered -> "ordered_resolution"
                                 | Ordered_with_fallback ->
                                     "ordered_resolution/fallback")
                              r
                          with
                          | Some d when d.clause_d = [] ->
                              stop_reason := Some (Refutation_found d)
                          | _ -> ()
                        end)
                      resolvents;

                    let superposed =
                      superpose_between mode given.clause_d other.clause_d
                    in
                    List.iter
                      (fun r ->
                        if !stop_reason = None then begin
                          incr superposition_inferences;
                          match
                            add_clause
                              ~to_support:true
                              ~parents:[ given.id; other.id ]
                              ~rule:"superposition"
                              r
                          with
                          | Some d when d.clause_d = [] ->
                              stop_reason := Some (Refutation_found d)
                          | _ -> ()
                        end)
                      superposed
                  end
                end)
              candidates
          end
  done;

  let stop_reason =
    match !stop_reason with
    | Some r -> r
    | None -> Saturation
  in

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
