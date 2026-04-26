open Types

let rec term_weight = function
  | Var _ -> 1
  | Fun (_, args) ->
      1 + List.fold_left (fun acc t -> acc + term_weight t) 0 args

let atom_weight a =
  1 + List.fold_left (fun acc t -> acc + term_weight t) 0 a.args

let compare_atom a b =
  let wa = atom_weight a in
  let wb = atom_weight b in
  if wa <> wb then compare wa wb
  else
    let c = compare a.pred b.pred in
    if c <> 0 then c else compare a.args b.args

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
          lit rest
      in
      clause
      |> List.mapi (fun i lit -> i, lit)
      |> List.filter (fun (_, lit) -> compare_literal lit max_lit = 0)
      |> List.map fst
