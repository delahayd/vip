open Types

let empty_subst : substitution = StringMap.empty

let apply_subst_term (s : substitution) t =
  let deref_var v =
    let rec loop seen v =
      if StringSet.mem v seen then
        Var v
      else
        match StringMap.find_opt v s with
        | None -> Var v
        | Some (Var v') -> loop (StringSet.add v seen) v'
        | Some t -> t
    in
    loop StringSet.empty v
  in
  let deref = function
    | Var v -> deref_var v
    | Fun _ as t -> t
  in
  let pop_n n out =
    let rec loop n acc out =
      if n = 0 then
        (acc, out)
      else
        match out with
        | [] -> invalid_arg "apply_subst_term: malformed reconstruction stack"
        | x :: xs -> loop (n - 1) (x :: acc) xs
    in
    loop n [] out
  in
  let rec push_args args rest =
    match args with
    | [] -> rest
    | x :: xs -> `Process x :: push_args xs rest
  in
  let rec loop stack out =
    match stack with
    | [] ->
        begin
          match out with
          | [ result ] -> result
          | _ -> invalid_arg "apply_subst_term: malformed result stack"
        end
    | `Build (f, n) :: stack ->
        let args, out = pop_n n out in
        loop stack (Fun (f, args) :: out)
    | `Process t :: stack ->
        match deref t with
        | Var _ as v -> loop stack (v :: out)
        | Fun (f, args) ->
            loop (push_args args (`Build (f, List.length args) :: stack)) out
  in
  loop [ `Process t ] []

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
