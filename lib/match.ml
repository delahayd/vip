open Types

exception Not_matchable

let rec match_terms t1 t2 subst =
  match t1, t2 with
  | Var v1, _ ->
      (match StringMap.find_opt v1 subst with
       | Some t -> if t = t2 then subst else raise Not_matchable
       | None ->
           StringMap.add v1 t2 subst)
  | Fun (f1, args1), Fun (f2, args2) ->
      if f1 = f2 && List.length args1 = List.length args2 then
        List.fold_left2 (fun s a1 a2 -> match_terms a1 a2 s) subst args1 args2
      else
        raise Not_matchable
  | _, _ -> raise Not_matchable

let match_atoms a1 a2 subst =
  if a1.pred = a2.pred && List.length a1.args = List.length a2.args then
    List.fold_left2 (fun s t1 t2 -> match_terms t1 t2 s) subst a1.args a2.args
  else
    raise Not_matchable

let match_literals l1 l2 subst =
  match l1, l2 with
  | Pos a1, Pos a2 | Neg a1, Neg a2 -> match_atoms a1 a2 subst
  | _, _ -> raise Not_matchable
