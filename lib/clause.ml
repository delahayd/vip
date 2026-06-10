open Types
open Subst

let rec term_size = function
  | Var _ -> 1
  | Fun (_, args) ->
      1 + List.fold_left (fun acc t -> acc + term_size t) 0 args

let atom_weight a =
  1 + List.fold_left (fun acc t -> acc + term_size t) 0 a.args

let literal_weight = function
  | Pos a | Neg a -> atom_weight a

let compare_term t1 t2 =

  match t1, t2 with
  | Var a, Var b -> compare a b
  | Var _, Fun _ -> -1
  | Fun _, Var _ -> 1
  | Fun (f1, a1), Fun (f2, a2) ->
      let s1 = term_size t1 in
      let s2 = term_size t2 in
      let csz = compare s1 s2 in
      if csz <> 0 then csz
      else
        let c = compare f1 f2 in
        if c <> 0 then c else compare a1 a2

let compare_atom a b =
  let wa = atom_weight a in
  let wb = atom_weight b in
  let cw = compare wa wb in
  if cw <> 0 then cw
  else
    let c = compare a.pred b.pred in
    if c <> 0 then c else compare a.args b.args

let compare_literal l1 l2 =
  match l1, l2 with
  | Pos a, Pos b -> compare_atom a b
  | Neg a, Neg b -> compare_atom a b
  | Pos a, Neg b ->
      let c = compare_atom a b in
      if c <> 0 then c else 1
  | Neg a, Pos b ->
      let c = compare_atom a b in
      if c <> 0 then c else -1

let rec canonicalize_term env next = function
  | Var v ->
      begin
        match StringMap.find_opt v env with
        | Some v' -> (Var v', env, next)
        | None ->
            let v' = "V" ^ string_of_int next in
            (Var v', StringMap.add v v' env, next + 1)
      end
  | Fun (f, args) ->
      let args', env, next =
        List.fold_left
          (fun (acc, env, next) t ->
            let t', env, next = canonicalize_term env next t in
            (t' :: acc, env, next))
          ([], env, next)
          args
      in
      (Fun (f, List.rev args'), env, next)

let canonicalize_atom env next a =
  let args', env, next =
    List.fold_left
      (fun (acc, env, next) t ->
        let t', env, next = canonicalize_term env next t in
        (t' :: acc, env, next))
      ([], env, next)
      a.args
  in
  ({ pred = a.pred; args = List.rev args' }, env, next)

let canonicalize_literal env next = function
  | Pos a ->
      let a', env, next = canonicalize_atom env next a in
      (Pos a', env, next)
  | Neg a ->
      let a', env, next = canonicalize_atom env next a in
      (Neg a', env, next)

let canonicalize_clause_vars c =
  let lits', _, _ =
    List.fold_left
      (fun (acc, env, next) lit ->
        let lit', env, next = canonicalize_literal env next lit in
        (lit' :: acc, env, next))
      ([], StringMap.empty, 0)
      c
  in
  List.rev lits'

let sort_uniq_literals lits =
  let xs = List.sort compare_literal lits in
  let rec dedup acc = function
    | a :: (b :: _ as tl) when compare_literal a b = 0 -> dedup acc tl
    | a :: tl -> dedup (a :: acc) tl
    | [] -> List.rev acc
  in
  dedup [] xs

let normalize_clause c =
  c
  |> sort_uniq_literals
  |> canonicalize_clause_vars
  |> sort_uniq_literals

let rec vars_of_term acc = function
  | Var v -> StringSet.add v acc
  | Fun (_, ts) -> List.fold_left vars_of_term acc ts

let vars_of_atom acc a =
  List.fold_left vars_of_term acc a.args

let vars_of_literal acc = function
  | Pos a | Neg a -> vars_of_atom acc a

let vars_of_clause c =
  List.fold_left vars_of_literal StringSet.empty c

let fresh_counter = ref 0

let reset_fresh_counter () =
  fresh_counter := 0

let fresh_var base =
  incr fresh_counter;
  base ^ "_" ^ string_of_int !fresh_counter

let rename_clause_apart (c : clause) : clause =
  let vars = vars_of_clause c in
  let subst =
    StringSet.fold
      (fun v acc -> StringMap.add v (Var (fresh_var v)) acc)
      vars
      StringMap.empty
  in
  apply_subst_clause subst c

let complementary l1 l2 =
  match l1, l2 with
  | Pos a, Neg b | Neg b, Pos a ->
      a.pred = b.pred && List.length a.args = List.length b.args
  | _ -> false

let is_true_lit = function
  | Pos { pred = "$true"; args = [] } -> true
  | Neg { pred = "$false"; args = [] } -> true
  | Pos { pred = "="; args = [ l; r ] } -> l = r
  | _ -> false

let is_false_lit = function
  | Pos { pred = "$false"; args = [] } -> true
  | Neg { pred = "$true"; args = [] } -> true
  | Neg { pred = "="; args = [ l; r ] } -> l = r
  | _ -> false

let clause_is_tautology c =
  List.exists is_true_lit c
  ||
  let rec aux = function
    | [] -> false
    | x :: xs ->
        List.exists
          (fun y ->
            match x, y with
            | Pos a, Neg b
            | Neg b, Pos a ->
                a = b
            | _ -> false)
          xs
        || aux xs
  in
  aux c

let simplify_clause c =
  if List.exists is_true_lit c then
    None
  else
    let c =
      c
      |> List.filter (fun lit -> not (is_false_lit lit))
      |> normalize_clause
    in
    if clause_is_tautology c then None else Some c

let vars_of_literal_list lit =
  vars_of_literal StringSet.empty lit

let var_literal_ownership c =
  let owners = Hashtbl.create 17 in
  List.iteri
    (fun i lit ->
      StringSet.iter
        (fun v ->
          match Hashtbl.find_opt owners v with
          | None -> Hashtbl.add owners v i
          | Some j when j = i -> ()
          | Some _ -> Hashtbl.replace owners v (-1))
        (vars_of_literal_list lit))
    c;
  owners

let fast_condense_once c =
  let len = List.length c in
  if len <= 1 then None
  else
    let owners = var_literal_ownership c in
    let rec try_deleted i =
      if i >= len then None
      else
        let deleted = List.nth c i in
        let deleted_vars = vars_of_literal_list deleted in
        let rec try_instance j =
          if j >= len then try_deleted (i + 1)
          else if i = j then try_instance (j + 1)
          else
            let instance = List.nth c j in
            match
              try Some (Match.match_literals deleted instance StringMap.empty)
              with Match.Not_matchable -> None
            with
            | None -> try_instance (j + 1)
            | Some subst ->
                let shared_vars_preserved =
                  StringSet.for_all
                    (fun v ->
                      match Hashtbl.find_opt owners v with
                      | Some owner when owner <> i ->
                          begin
                            match StringMap.find_opt v subst with
                            | None -> true
                            | Some (Var v') -> v = v'
                            | Some _ -> false
                          end
                      | _ -> true)
                    deleted_vars
                in
                if shared_vars_preserved then
                  let kept = List.filteri (fun k _ -> k <> i) c in
                  Some kept
                else
                  try_instance (j + 1)
        in
        try_instance 0
    in
    try_deleted 0

let fast_condense_clause c =
  let rec loop c =
    match fast_condense_once c with
    | None -> simplify_clause c
    | Some c' ->
        match simplify_clause c' with
        | None -> None
        | Some c' -> loop c'
  in
  loop c

let subsumes c1 c2 =
  let n = List.length c1 in
  let m = List.length c2 in
  if n > m then false
  else
    let remove_nth xs n =
      xs |> List.filteri (fun i _ -> i <> n)
    in
    let rec try_match_subset subst remaining_targets = function
      | [] -> true
      | l1 :: rest_c1 ->
          let rec search i = function
            | [] -> false
            | l2 :: rest_targets ->
                (try
                   let subst' = Match.match_literals l1 l2 subst in
                   let remaining_targets' = remove_nth remaining_targets i in
                   try_match_subset subst' remaining_targets' rest_c1
                   || search (i + 1) rest_targets
                 with Match.Not_matchable -> search (i + 1) rest_targets)
          in
          search 0 remaining_targets
    in
    try_match_subset StringMap.empty c2 c1

let subsumption_resolution c1 c2 =
  let n = List.length c1 in
  let m = List.length c2 in
  if n > m then None
  else
    (* For C1 = L v C1' to subsumption-resolve C2 = L2 v C2', 
       L2 must be the complement of Lθ, and C1'θ must be a subset of C2'. *)
    let rec find_resolving_lit i1 =
      if i1 = n then None
      else
        let l1 = List.nth c1 i1 in
        let c1_rest = List.filteri (fun k _ -> k <> i1) c1 in
        
        let rec find_target_lit i2 =
          if i2 = m then None
          else
            let l2 = List.nth c2 i2 in
            match l1, l2 with
            | Pos a1, Neg a2 | Neg a1, Pos a2 ->
                (try
                  let subst = Match.match_atoms a1 a2 StringMap.empty in
                  let c2_rest = List.filteri (fun k _ -> k <> i2) c2 in
                  if subsumes (apply_subst_clause subst c1_rest) c2_rest then
                    Some c2_rest
                  else
                    find_target_lit (i2 + 1)
                 with Match.Not_matchable -> find_target_lit (i2 + 1))
            | _ -> find_target_lit (i2 + 1)
        in
        match find_target_lit 0 with
        | Some res -> Some res
        | None -> find_resolving_lit (i1 + 1)
    in
    find_resolving_lit 0

let maximal_literal_indices c =
  match c with
  | [] -> []
  | _ ->
      let max_w =
        List.fold_left
          (fun acc lit -> max acc (literal_weight lit))
          (literal_weight (List.hd c))
          (List.tl c)
      in
      let rec collect i acc = function
        | [] -> List.rev acc
        | lit :: tl ->
            let acc =
              if literal_weight lit = max_w then i :: acc else acc
            in
            collect (i + 1) acc tl
      in
      collect 0 [] c
