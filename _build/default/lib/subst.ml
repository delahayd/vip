open Types

let empty_subst : substitution = StringMap.empty

let rec apply_subst_term (s : substitution) = function
  | Var v ->
      begin
        match StringMap.find_opt v s with
        | None -> Var v
        | Some t -> apply_subst_term s t
      end
  | Fun (f, args) -> Fun (f, List.map (apply_subst_term s) args)

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
