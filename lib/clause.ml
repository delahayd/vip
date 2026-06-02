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

let clause_is_tautology c =
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

let is_true_lit = function
  | Pos { pred = "$true"; args = [] } -> true
  | Neg { pred = "$false"; args = [] } -> true
  | _ -> false

let is_false_lit = function
  | Pos { pred = "$false"; args = [] } -> true
  | Neg { pred = "$true"; args = [] } -> true
  | _ -> false

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

let subsumes c1 c2 =
  let n = List.length c1 in
  let m = List.length c2 in
  if n > m then false
  else
    (* Try to find a matching substitution for each literal of c1 into some literal of c2 *)
    let rec try_match_subset subst idx =
      if idx = n then true
      else
        let l1 = List.nth c1 idx in
        let rec search_in_c2 = function
          | [] -> false
          | l2 :: rest ->
              (try
                let subst' = Match.match_literals l1 l2 subst in
                if try_match_subset subst' (idx + 1) then true
                else search_in_c2 rest
               with Match.Not_matchable -> search_in_c2 rest)
        in
        search_in_c2 c2
    in
    try_match_subset StringMap.empty 0

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
