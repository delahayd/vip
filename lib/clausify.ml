open Types
open Fof
open Clause

type clausification_report = {
  input_count : int;
  produced_clause_count : int;
}

type partitioned_clauses = {
  axioms : clause list;
  support : clause list;
  report : clausification_report;
}

exception Timeout_hit

let current_check_timeout = ref (fun () -> ())

let poll_timeout () = !current_check_timeout ()

let with_timeout_poll check_timeout f =
  let previous = !current_check_timeout in
  current_check_timeout := check_timeout;
  Fun.protect
    ~finally:(fun () -> current_check_timeout := previous)
    f

let fresh_counter = ref 0
let skolem_counter = ref 0
let def_counter = ref 0

let reset_fresh_state () =
  fresh_counter := 0;
  skolem_counter := 0;
  def_counter := 0

let fresh_var base =
  incr fresh_counter;
  base ^ "_u" ^ string_of_int !fresh_counter

let fresh_skolem_name () =
  incr skolem_counter;
  "sk" ^ string_of_int !skolem_counter

let fresh_def_name () =
  incr def_counter;
  "ip_def_" ^ string_of_int !def_counter

let rec elim_imp f =
  poll_timeout ();
  match f with
  | FTrue -> FTrue
  | FFalse -> FFalse
  | Atom _ as a -> a
  | Not f -> Not (elim_imp f)
  | And (a, b) -> And (elim_imp a, elim_imp b)
  | Or (a, b) -> Or (elim_imp a, elim_imp b)
  | Imp (a, b) -> Or (Not (elim_imp a), elim_imp b)
  | RevImp (a, b) -> Or (elim_imp a, Not (elim_imp b))
  | Iff (a, b) ->
      let a' = elim_imp a in
      let b' = elim_imp b in
      And (Or (Not a', b'), Or (Not b', a'))
  | Xor (a, b) ->
      let a' = elim_imp a in
      let b' = elim_imp b in
      Or (And (a', Not b'), And (Not a', b'))
  | Forall (vs, f) -> Forall (vs, elim_imp f)
  | Exists (vs, f) -> Exists (vs, elim_imp f)

let rec nnf f =
  poll_timeout ();
  match f with
  | FTrue -> FTrue
  | FFalse -> FFalse
  | Atom _ as a -> a
  | Not FTrue -> FFalse
  | Not FFalse -> FTrue
  | Not (Atom a) -> Not (Atom a)
  | Not (Not f) -> nnf f
  | Not (And (a, b)) -> Or (nnf (Not a), nnf (Not b))
  | Not (Or (a, b)) -> And (nnf (Not a), nnf (Not b))
  | Not (Forall (vs, f)) -> Exists (vs, nnf (Not f))
  | Not (Exists (vs, f)) -> Forall (vs, nnf (Not f))
  | Not f -> nnf (Not (elim_imp f))
  | And (a, b) -> And (nnf a, nnf b)
  | Or (a, b) -> Or (nnf a, nnf b)
  | Forall (vs, f) -> Forall (vs, nnf f)
  | Exists (vs, f) -> Exists (vs, nnf f)
  | Imp _ | RevImp _ | Iff _ | Xor _ as f -> nnf (elim_imp f)

let rec standardize bound f =
  poll_timeout ();
  match f with
  | FTrue -> FTrue
  | FFalse -> FFalse
  | Atom _ as a -> a
  | Not f -> Not (standardize bound f)
  | And (a, b) -> And (standardize bound a, standardize bound b)
  | Or (a, b) -> Or (standardize bound a, standardize bound b)
  | Forall (vs, f) ->
      let renamings, vs' =
        List.fold_left
          (fun (subs, acc) v ->
            let v' = fresh_var v in
            ((v, Var v') :: subs, acc @ [v']))
          ([], [])
          vs
      in
      let f' = subst_formula renamings f in
      Forall (vs', standardize (vs' @ bound) f')
  | Exists (vs, f) ->
      let renamings, vs' =
        List.fold_left
          (fun (subs, acc) v ->
            let v' = fresh_var v in
            ((v, Var v') :: subs, acc @ [v']))
          ([], [])
          vs
      in
      let f' = subst_formula renamings f in
      Exists (vs', standardize (vs' @ bound) f')
  | Imp _ | RevImp _ | Iff _ | Xor _ ->
      failwith "standardize: connecteur inattendu"

let skolem_term universals =
  let name = fresh_skolem_name () in
  match universals with
  | [] -> Fun (name, [])
  | vs -> Fun (name, List.map (fun v -> Var v) vs)

let rec skolem universals f =
  poll_timeout ();
  match f with
  | FTrue -> FTrue
  | FFalse -> FFalse
  | Atom _ as a -> a
  | Not _ as n -> n
  | And (a, b) -> And (skolem universals a, skolem universals b)
  | Or (a, b) -> Or (skolem universals a, skolem universals b)
  | Forall (vs, f) -> Forall (vs, skolem (universals @ vs) f)
  | Exists (vs, f) ->
      let replacements = List.map (fun v -> (v, skolem_term universals)) vs in
      skolem universals (subst_formula replacements f)
  | Imp _ | RevImp _ | Iff _ | Xor _ ->
      failwith "skolem: formule non normalisée"

let rec drop_forall f =
  poll_timeout ();
  match f with
  | Forall (_, f) -> drop_forall f
  | And (a, b) -> And (drop_forall a, drop_forall b)
  | Or (a, b) -> Or (drop_forall a, drop_forall b)
  | Not f -> Not (drop_forall f)
  | (FTrue | FFalse | Atom _) as f -> f
  | Exists _ -> failwith "drop_forall: il reste un existentiel"
  | Imp _ | RevImp _ | Iff _ | Xor _ ->
      failwith "drop_forall: formule non préparée"

let split_top_level_conjuncts f =
  let rec aux wrappers acc f =
    poll_timeout ();
    match f with
    | And (a, b) ->
        aux wrappers (aux wrappers acc b) a
    | Forall (vs, body) ->
        aux (fun part -> wrappers (Forall (vs, part))) acc body
    | _ -> wrappers f :: acc
  in
  aux (fun x -> x) [] f

let rec distribute a b =
  poll_timeout ();
  match a, b with
  | And (a1, a2), x -> And (distribute a1 x, distribute a2 x)
  | x, And (b1, b2) -> And (distribute x b1, distribute x b2)
  | x, y -> Or (x, y)

let rec cnf_shape f =
  poll_timeout ();
  match f with
  | And (a, b) -> And (cnf_shape a, cnf_shape b)
  | Or (a, b) -> distribute (cnf_shape a) (cnf_shape b)
  | (Atom _ | Not (Atom _) | FTrue | FFalse) as f -> f
  | Not _ -> failwith "cnf_shape: négation non atomique"
  | Forall _ | Exists _ | Imp _ | RevImp _ | Iff _ | Xor _ ->
      failwith "cnf_shape: formule non préparée"

let lit_of_formula = function
  | Atom a -> Some (Pos a)
  | Not (Atom a) -> Some (Neg a)
  | FFalse -> None
  | FTrue -> failwith "FTrue dans une clause"
  | _ -> failwith "formule non littérale"

let rec flatten_or f =
  let rec aux acc = function
    | Or (a, b) ->
        poll_timeout ();
        aux (aux acc b) a
    | x ->
        poll_timeout ();
        x :: acc
  in
  aux [] f

let rec flatten_and f =
  let rec aux acc = function
    | And (a, b) ->
        poll_timeout ();
        aux (aux acc b) a
    | x ->
        poll_timeout ();
        x :: acc
  in
  aux [] f

let clause_of_disjunction f =
  let parts = flatten_or f in
  if List.exists (function FTrue -> true | _ -> false) parts then
    None
  else
    let lits =
      parts
      |> List.filter (function FFalse -> false | _ -> true)
      |> List.map lit_of_formula
      |> List.filter_map (fun x -> x)
    in
    simplify_clause lits

let clauses_of_cnf_formula f =
  flatten_and f |> List.filter_map clause_of_disjunction

let neg_literal = function
  | Pos a -> Neg a
  | Neg a -> Pos a

let true_lit = Pos { pred = "$true"; args = [] }
let false_lit = Pos { pred = "$false"; args = [] }

let definition_literal f =
  let vars =
    Fof.vars_of_formula Types.StringSet.empty f
    |> Types.StringSet.elements
  in
  Pos { pred = fresh_def_name (); args = List.map (fun v -> Var v) vars }

let definitional_clauses_of_nnf f =
  let rec aux f =
    poll_timeout ();
    match f with
    | FTrue -> (true_lit, [])
    | FFalse -> (false_lit, [])
    | Atom a -> (Pos a, [])
    | Not (Atom a) -> (Neg a, [])
    | And (a, b) ->
        let la, ca = aux a in
        let lb, cb = aux b in
        let d = definition_literal f in
        ( d,
          ca @ cb
          @ [
              [ neg_literal d; la ];
              [ neg_literal d; lb ];
              [ d; neg_literal la; neg_literal lb ];
            ] )
    | Or (a, b) ->
        let la, ca = aux a in
        let lb, cb = aux b in
        let d = definition_literal f in
        ( d,
          ca @ cb
          @ [
              [ neg_literal d; la; lb ];
              [ d; neg_literal la ];
              [ d; neg_literal lb ];
            ] )
    | Not _ -> failwith "definitional CNF: négation non atomique"
    | Forall _ | Exists _ | Imp _ | RevImp _ | Iff _ | Xor _ ->
        failwith "definitional CNF: formule non préparée"
  in
  let top, clauses = aux f in
  clauses @ [ [ top ] ]
  |> List.filter_map simplify_clause

let getenv_bool name default =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON") -> true
  | Some ("0" | "false" | "FALSE" | "no" | "NO" | "off" | "OFF") -> false
  | _ -> default

let getenv_int name default =
  match Sys.getenv_opt name with
  | Some value -> (
      match int_of_string_opt value with
      | Some n when n > 0 -> n
      | _ -> default)
  | None -> default

let saturated_add limit a b =
  if a >= limit || b >= limit || a > limit - b then limit else a + b

let saturated_mul limit a b =
  if a = 0 || b = 0 then 0
  else if a >= limit || b >= limit || a > limit / b then limit
  else a * b

let estimated_cnf_clause_count ~limit f =
  let rec aux f =
    poll_timeout ();
    match f with
    | And (a, b) -> saturated_add limit (aux a) (aux b)
    | Or (a, b) -> saturated_mul limit (aux a) (aux b)
    | Atom _ | Not (Atom _) | FTrue | FFalse -> 1
    | Not _ -> limit
    | Forall _ | Exists _ | Imp _ | RevImp _ | Iff _ | Xor _ -> limit
  in
  aux f

let clausify_formula ?(check_timeout = fun () -> ()) f =
  with_timeout_poll check_timeout (fun () ->
      let prepared =
        f
        |> elim_imp
        |> nnf
        |> standardize []
        |> skolem []
        |> drop_forall
      in
      let force_definitional = getenv_bool "IP_DEFINITIONAL_CNF" false in
      let auto_definitional = getenv_bool "IP_AUTO_DEFINITIONAL_CNF" false in
      let distribution_limit = getenv_int "IP_CNF_DISTRIBUTION_LIMIT" 4096 in
      let use_definitional =
        force_definitional
        || (auto_definitional
            && estimated_cnf_clause_count
                 ~limit:(distribution_limit + 1)
                 prepared
               > distribution_limit)
      in
      if use_definitional then
        definitional_clauses_of_nnf prepared
      else
        prepared
        |> cnf_shape
        |> clauses_of_cnf_formula)

let role_requires_negation role =
  role = "conjecture"

let is_support_role role =
  role = "conjecture" || role = "negated_conjecture"

let clauses_of_input_with_report ?(check_timeout = fun () -> ()) inputs =
  with_timeout_poll check_timeout (fun () ->
  let rec aux acc produced = function
    | [] ->
        (List.rev acc, { input_count = List.length inputs; produced_clause_count = produced })
    | Input_include _ :: xs ->
        aux acc produced xs
    | Input_cnf { role; clause; _ } :: xs ->
        let raw_clauses =
          if role_requires_negation role then
            List.map
              (fun lit -> [match lit with Pos a -> Neg a | Neg a -> Pos a])
              clause
          else
            [clause]
        in
        let clauses = raw_clauses |> List.filter_map simplify_clause in
        aux (List.rev_append clauses acc) (produced + List.length clauses) xs
    | Input_fof { role; formula; _ } :: xs ->
        let f = if role_requires_negation role then Not formula else formula in
        let formulas =
          if role_requires_negation role then [ f ] else split_top_level_conjuncts f
        in
        let cls = List.concat_map (clausify_formula ~check_timeout) formulas in
        aux (List.rev_append cls acc) (produced + List.length cls) xs
  in
  aux [] 0 inputs)

let clauses_of_input ?(check_timeout = fun () -> ()) inputs =
  fst (clauses_of_input_with_report ~check_timeout inputs)

let partition_input_clauses ?(check_timeout = fun () -> ()) inputs =
  with_timeout_poll check_timeout (fun () ->
  let rec aux axioms support produced = function
    | [] ->
        {
          axioms = List.rev axioms;
          support = List.rev support;
          report = {
            input_count = List.length inputs;
            produced_clause_count = produced;
          };
        }
    | Input_include _ :: xs ->
        aux axioms support produced xs
    | Input_cnf { role; clause; _ } :: xs ->
        let raw_clauses =
          if role_requires_negation role then
            List.map
              (fun lit -> [match lit with Pos a -> Neg a | Neg a -> Pos a])
              clause
          else
            [clause]
        in
        let clauses = raw_clauses |> List.filter_map simplify_clause in
        let axioms, support =
          if is_support_role role then
            (axioms, List.rev_append clauses support)
          else
            (List.rev_append clauses axioms, support)
        in
        aux axioms support (produced + List.length clauses) xs
    | Input_fof { role; formula; _ } :: xs ->
        let f = if role_requires_negation role then Not formula else formula in
        let formulas =
          if role_requires_negation role then [ f ] else split_top_level_conjuncts f
        in
        let cls = List.concat_map (clausify_formula ~check_timeout) formulas in
        let axioms, support =
          if is_support_role role then
            (axioms, List.rev_append cls support)
          else
            (List.rev_append cls axioms, support)
        in
        aux axioms support (produced + List.length cls) xs
  in
  aux [] [] 0 inputs)
