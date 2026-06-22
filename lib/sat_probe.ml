open Types

type result =
  | Satisfiable
  | Not_applicable of string
  | Too_large of string
  | Unsat

let rec term_has_variable = function
  | Var _ -> true
  | Fun (_, args) -> List.exists term_has_variable args

let rec collect_signature_term (constants, has_nonconst_fun) = function
  | Var _ -> (constants, has_nonconst_fun)
  | Fun (f, []) -> (Types.StringSet.add f constants, has_nonconst_fun)
  | Fun (_, args) ->
      List.fold_left collect_signature_term (constants, true) args

let collect_signature_atom acc a =
  List.fold_left collect_signature_term acc a.args

let collect_signature_literal acc = function
  | Pos a | Neg a -> collect_signature_atom acc a

let collect_signature clauses =
  List.fold_left
    (fun acc c -> List.fold_left collect_signature_literal acc c)
    (Types.StringSet.empty, false)
    clauses

let vars_of_clause c = Clause.vars_of_clause c |> Types.StringSet.elements

let rec instantiate_term subst = function
  | Var v ->
      begin
        match Types.StringMap.find_opt v subst with
        | Some t -> t
        | None -> Var v
      end
  | Fun (f, args) -> Fun (f, List.map (instantiate_term subst) args)

let instantiate_atom subst a =
  { a with args = List.map (instantiate_term subst) a.args }

let instantiate_literal subst = function
  | Pos a -> Pos (instantiate_atom subst a)
  | Neg a -> Neg (instantiate_atom subst a)

let instantiate_clause subst c =
  List.map (instantiate_literal subst) c

let ground_instances ~max_instances ~check_timeout constants c =
  let vars = vars_of_clause c in
  match vars with
  | [] ->
      `Ok [ c ]
  | _ ->
      let produced = ref 0 in
      let rec loop subst = function
        | [] ->
            check_timeout ();
            incr produced;
            if !produced > max_instances then
              `Too_large
            else
              `Ok [ instantiate_clause subst c ]
        | v :: rest ->
            let rec try_constants acc = function
              | [] -> `Ok (List.rev acc)
              | const :: tl ->
                  check_timeout ();
                  let subst' = Types.StringMap.add v const subst in
                  match loop subst' rest with
                  | `Too_large -> `Too_large
                  | `Ok instances -> try_constants (List.rev_append instances acc) tl
            in
            try_constants [] constants
      in
      loop Types.StringMap.empty vars

let atom_key a = { a with args = a.args }

let encode_ground_clause sat atom_vars c =
  let clause_lits = ref [] in
  let get_var atom =
    match Hashtbl.find_opt atom_vars atom with
    | Some v -> v
    | None ->
        let v = Prop_sat.new_var sat in
        Hashtbl.add atom_vars atom v;
        v
  in
  List.iter
    (function
      | Pos { pred = "$true"; args = [] } ->
          clause_lits := [ Prop_sat.Pos (Prop_sat.new_var sat) ];
          Prop_sat.add_clause sat !(clause_lits)
      | Neg { pred = "$false"; args = [] } ->
          clause_lits := [ Prop_sat.Pos (Prop_sat.new_var sat) ];
          Prop_sat.add_clause sat !(clause_lits)
      | Pos a ->
          clause_lits := Prop_sat.Pos (get_var (atom_key a)) :: !clause_lits
      | Neg a ->
          clause_lits := Prop_sat.Neg (get_var (atom_key a)) :: !clause_lits)
    c;
  Prop_sat.add_clause sat !(clause_lits)

let has_equality clauses =
  List.exists
    (List.exists
       (function
         | Pos { pred = "="; _ } | Neg { pred = "="; _ } -> true
         | _ -> false))
    clauses

let run ?(max_instances = 20_000) ?(check_timeout = fun () -> ()) clauses =
  check_timeout ();
  if clauses = [] then
    Satisfiable
  else if has_equality clauses then
    Not_applicable "equality requires finite-model congruence handling"
  else
    let constants, has_nonconst_fun = collect_signature clauses in
    if has_nonconst_fun then
      Not_applicable "non-constant function symbols"
    else
      let constants =
        let xs = Types.StringSet.elements constants in
        if xs = [] then [ Fun ("c_sat_probe", []) ]
        else List.map (fun c -> Fun (c, [])) xs
      in
      let rec instantiate_all acc = function
        | [] -> `Ok (List.rev acc)
        | c :: tl ->
            begin
              match ground_instances ~max_instances ~check_timeout constants c with
              | `Too_large -> `Too_large
              | `Ok instances -> instantiate_all (List.rev_append instances acc) tl
            end
      in
      match instantiate_all [] clauses with
      | `Too_large ->
          Too_large "ground instance limit reached"
      | `Ok ground_clauses ->
          let sat = Prop_sat.create () in
          let atom_vars = Hashtbl.create 257 in
          let add_clause c =
            check_timeout ();
            match Clause.simplify_clause c with
            | None -> ()
            | Some [] -> Prop_sat.add_clause sat []
            | Some c -> encode_ground_clause sat atom_vars c
          in
          List.iter add_clause ground_clauses;
          if Prop_sat.satisfiable sat [] then Satisfiable else Unsat
