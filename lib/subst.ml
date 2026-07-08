open Types

let empty_subst : substitution = StringMap.empty

let apply_subst_term (s : substitution) t =
  let rec aux seen = function
    | Var v ->
        if StringSet.mem v seen then
          Var v
        else
          begin
            match StringMap.find_opt v s with
            | None -> Var v
            | Some t -> aux (StringSet.add v seen) t
          end
    | Fun (f, args) -> Fun (f, List.map (aux seen) args)
  in
  aux StringSet.empty t

let apply_subst_atom s a =
  { a with args = List.map (apply_subst_term s) a.args }

let apply_subst_literal s = function
  | Pos a -> Pos (apply_subst_atom s a)
  | Neg a -> Neg (apply_subst_atom s a)

let apply_subst_clause s c =
  List.map (apply_subst_literal s) c

let compose_subst s1 s2 =
  let s2' = StringMap.map (apply_subst_term s1) s2 in
  StringMap.union (fun _ a _ -> Some a) s1 s2'

let rec occurs v = function
  | Var x -> x = v
  | Fun (_, ts) -> List.exists (occurs v) ts
