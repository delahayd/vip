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

let negative_indices c =
  c
  |> List.mapi (fun i lit -> i, lit)
  |> List.filter (function _, Neg _ -> true | _ -> false)
  |> List.map fst

let all_indices c =
  let rec aux i acc = function
    | [] -> List.rev acc
    | _ :: tl -> aux (i + 1) (i :: acc) tl
  in
  aux 0 [] c

let selected_or_maximal_indices c =
  let negs = negative_indices c in
  if negs <> [] then negs
  else Ordering.maximal_literal_indices c

let dedup_clauses cls =
  let sorted = List.sort compare cls in
  let rec aux acc = function
    | a :: (b :: _ as tl) when a = b -> aux acc tl
    | a :: tl -> aux (a :: acc) tl
    | [] -> List.rev acc
  in
  aux [] sorted

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
        let resolvent = apply_subst_clause s (rem1 @ rem2) in
        simplify_clause resolvent
      with Not_unifiable ->
        None
    else
      None
  else
    None

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
  | Unrestricted ->
      resolve_unrestricted c1 c2
  | Ordered ->
      resolve_ordered_strict c1 c2
  | Ordered_with_fallback ->
      let ordered = resolve_ordered_strict c1 c2 in
      if ordered <> [] then ordered else resolve_unrestricted c1 c2

let ordered_factor_clause c =
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
                  let factored = apply_subst_clause s kept in
                  match simplify_clause factored with
                  | Some c -> results := c :: !results
                  | None -> ()
                with Not_unifiable -> ())
        (all_indices c))
    active;
  dedup_clauses !results

let run_resolution_sos ?(limits = default_limits) ~mode ~axioms ~support () =
  let start_t = Unix.gettimeofday () in

  let known : (string, int) Hashtbl.t = Hashtbl.create 4099 in
  let all_by_id : (int, derived) Hashtbl.t = Hashtbl.create 4099 in
  let agenda : derived list ref = ref [] in
  let all : derived list ref = ref [] in

  let generated = ref 0 in
  let processed = ref 0 in

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
      if na <> nb then na
      else a.id < b.id
  in

  let rec insert_agenda d = function
    | [] -> [d]
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
    List.exists (fun d -> subsumes d.clause_d c) (existing_clauses ())
  in

  let add_clause ~to_support ~parents ~rule c =
    match simplify_clause c with
    | None -> None
    | Some c ->
        let key = string_of_clause c in
        if Hashtbl.mem known key || is_subsumed_by_existing c then
          None
        else begin
          incr generated;
          let d =
            {
              id = next_id ();
              parents;
              rule;
              clause_d = c;
            }
          in
          Hashtbl.add known key d.id;
          Hashtbl.replace all_by_id d.id d;
          all := d :: !all;
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
    (fun c ->
      if !stop_reason = None then add_initial ~to_support:false c)
    axioms;

  List.iter
    (fun c ->
      if !stop_reason = None then add_initial ~to_support:true c)
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

            let factors =
              match mode with
              | Unrestricted -> ordered_factor_clause given.clause_d
              | Ordered | Ordered_with_fallback -> ordered_factor_clause given.clause_d
            in

            List.iter
              (fun fc ->
                if !stop_reason = None then
                  match
                    add_clause
                      ~to_support:true
                      ~parents:[given.id]
                      ~rule:"factor"
                      fc
                  with
                  | Some d when d.clause_d = [] ->
                      stop_reason := Some (Refutation_found d)
                  | _ -> ())
              factors;

            let candidates = existing_clauses () in

            List.iter
              (fun other ->
                if !stop_reason = None && Hashtbl.mem all_by_id other.id then begin
                  if time_exceeded () then
                    stop_reason := Some Time_limit
                  else if clause_limit_exceeded () then
                    stop_reason := Some Clause_limit
                  else
                    let resolvents =
                      resolve_by_mode mode given.clause_d other.clause_d
                    in
                    List.iter
                      (fun r ->
                        if !stop_reason = None then
                          match
                            add_clause
                              ~to_support:true
                              ~parents:[given.id; other.id]
                              ~rule:
                                (match mode with
                                 | Unrestricted -> "resolution"
                                 | Ordered -> "ordered_resolution"
                                 | Ordered_with_fallback -> "ordered_resolution/fallback")
                              r
                          with
                          | Some d when d.clause_d = [] ->
                              stop_reason := Some (Refutation_found d)
                          | _ -> ())
                      resolvents
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
    stats =
      {
        generated_clauses = !generated;
        processed_clauses = !processed;
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
        | ps ->
            String.concat "," (List.map string_of_int ps)
      in
      Printf.printf
        "%4d. %-28s [%s] %s\n"
        d.id
        d.rule
        parents
        (string_of_clause d.clause_d))
    deriveds
