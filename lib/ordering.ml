open Types

module SMap = Types.StringMap

let precedence_table : (string, int) Hashtbl.t = Hashtbl.create 251

let clear_precedence () =
  Hashtbl.clear precedence_table

let set_precedence symbols =
  clear_precedence ();
  let n = List.length symbols in
  List.iteri
    (fun i s ->
      Hashtbl.replace precedence_table s (n - i))
    symbols

let precedence_rank s =
  match Hashtbl.find_opt precedence_table s with
  | Some r -> r
  | None -> 0

let compare_symbol_precedence a b =
  let ra = precedence_rank a in
  let rb = precedence_rank b in
  if ra <> rb then compare ra rb
  else compare a b

let symbol_weight _ = 1
let variable_weight _ = 1

let rec term_weight = function
  | Var v -> variable_weight v
  | Fun (f, args) ->
      symbol_weight f
      + List.fold_left (fun acc t -> acc + term_weight t) 0 args

let add_var v m =
  SMap.update v
    (function
      | None -> Some 1
      | Some n -> Some (n + 1))
    m

let rec var_counts_term acc = function
  | Var v -> add_var v acc
  | Fun (_, args) ->
      List.fold_left var_counts_term acc args

let var_count_ge s t =
  let cs = var_counts_term SMap.empty s in
  let ct = var_counts_term SMap.empty t in
  SMap.for_all
    (fun v nt ->
      let ns =
        match SMap.find_opt v cs with
        | Some n -> n
        | None -> 0
      in
      ns >= nt)
    ct

let rec structural_compare_term a b =
  match a, b with
  | Var x, Var y -> compare x y
  | Var _, Fun _ -> -1
  | Fun _, Var _ -> 1
  | Fun (f, xs), Fun (g, ys) ->
      let c = compare f g in
      if c <> 0 then c else compare xs ys

let rec lex_kbo_gt xs ys =
  match xs, ys with
  | [], [] -> false
  | [], _ -> false
  | _, [] -> true
  | x :: xs, y :: ys ->
      if x = y then lex_kbo_gt xs ys
      else kbo_gt x y

and kbo_gt_same_weight s t =
  match s, t with
  | Var _, _ -> false
  | Fun _, Var _ -> true
  | Fun (f, xs), Fun (g, ys) ->
      let c = compare_symbol_precedence f g in
      if c <> 0 then c > 0
      else lex_kbo_gt xs ys

and kbo_gt s t =
  s <> t
  && var_count_ge s t
  &&
  let ws = term_weight s in
  let wt = term_weight t in
  ws > wt || (ws = wt && kbo_gt_same_weight s t)

let compare_term_kbo s t =
  if s = t then 0
  else if kbo_gt s t then 1
  else if kbo_gt t s then -1
  else structural_compare_term s t

let atom_weight a =
  1 + List.fold_left (fun acc t -> acc + term_weight t) 0 a.args

let rec lex_compare_terms xs ys =
  match xs, ys with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let c = compare_term_kbo x y in
      if c <> 0 then c else lex_compare_terms xs ys

let compare_atom a b =
  let wa = atom_weight a in
  let wb = atom_weight b in
  if wa <> wb then compare wa wb
  else
    let c = compare_symbol_precedence a.pred b.pred in
    if c <> 0 then c else lex_compare_terms a.args b.args

let compare_literal l1 l2 =
  match l1, l2 with
  | Pos a, Pos b -> compare_atom a b
  | Neg a, Neg b -> compare_atom a b
  | Neg a, Pos b ->
      let c = compare_atom a b in
      if c <> 0 then c else 1
  | Pos a, Neg b ->
      let c = compare_atom a b in
      if c <> 0 then c else -1

let maximal_literal_indices clause =
  match clause with
  | [] -> []
  | lit :: rest ->
      let max_lit =
        List.fold_left
          (fun acc lit ->
            if compare_literal lit acc > 0 then lit else acc)
          lit
          rest
      in
      clause
      |> List.mapi (fun i lit -> i, lit)
      |> List.filter (fun (_, lit) -> compare_literal lit max_lit = 0)
      |> List.map fst
