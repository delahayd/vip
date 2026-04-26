open Types
open Subst

exception Not_unifiable

let bind_var v t s =
  let t = apply_subst_term s t in
  if t = Var v then s
  else if occurs v t then raise Not_unifiable
  else
    let singleton = StringMap.singleton v t in
    compose_subst singleton s

let rec unify_terms t1 t2 s =
  let t1 = apply_subst_term s t1 in
  let t2 = apply_subst_term s t2 in
  match t1, t2 with
  | Var v, t | t, Var v -> bind_var v t s
  | Fun (f1, a1), Fun (f2, a2) ->
      if f1 <> f2 || List.length a1 <> List.length a2 then
        raise Not_unifiable;
      List.fold_left2 (fun acc x y -> unify_terms x y acc) s a1 a2

let unify_atoms a b s =
  if a.pred <> b.pred || List.length a.args <> List.length b.args then
    raise Not_unifiable;
  List.fold_left2 (fun acc x y -> unify_terms x y acc) s a.args b.args
