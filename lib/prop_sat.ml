type lit =
  | Pos of int
  | Neg of int

type t = {
  mutable next_var : int;
  clauses : lit list list ref;
}

let create () = { next_var = 0; clauses = ref [] }

let new_var st =
  st.next_var <- st.next_var + 1;
  st.next_var

let normalize_clause clause =
  clause |> List.sort_uniq compare

let add_clause st clause =
  st.clauses := normalize_clause clause :: !(st.clauses)

let var_of_lit = function Pos v | Neg v -> v
let sign_of_lit = function Pos _ -> true | Neg _ -> false

let eval_lit assign lit =
  match Hashtbl.find_opt assign (var_of_lit lit) with
  | None -> None
  | Some v -> Some (v = sign_of_lit lit)

let set_lit assign lit =
  let var = var_of_lit lit in
  let value = sign_of_lit lit in
  match Hashtbl.find_opt assign var with
  | None ->
      Hashtbl.add assign var value;
      true
  | Some old ->
      old = value

let vars_of_clauses clauses =
  clauses
  |> List.concat
  |> List.map var_of_lit
  |> List.sort_uniq compare

let rec propagate clauses assign =
  let changed = ref false in
  let conflict = ref false in
  List.iter
    (fun clause ->
      if not !conflict then begin
        let satisfied = ref false in
        let unassigned = ref [] in
        List.iter
          (fun lit ->
            match eval_lit assign lit with
            | Some true -> satisfied := true
            | Some false -> ()
            | None -> unassigned := lit :: !unassigned)
          clause;
        if not !satisfied then
          match !unassigned with
          | [] -> conflict := true
          | [ lit ] ->
              if set_lit assign lit then changed := true
              else conflict := true
          | _ -> ()
      end)
    clauses;
  if !conflict then false
  else if !changed then propagate clauses assign
  else true

let copy_assign assign =
  let copy = Hashtbl.create (Hashtbl.length assign) in
  Hashtbl.iter (fun k v -> Hashtbl.add copy k v) assign;
  copy

let model ?(prefer_false = false) st assumptions =
  let clauses = List.map (fun lit -> [ lit ]) assumptions @ !(st.clauses) in
  let vars = vars_of_clauses clauses in
  let rec search assign =
    if not (propagate clauses assign) then None
    else
      match List.find_opt (fun v -> not (Hashtbl.mem assign v)) vars with
      | None -> Some assign
      | Some v ->
          let first_value, second_value =
            if prefer_false then (false, true) else (true, false)
          in
          let assign_first = copy_assign assign in
          Hashtbl.add assign_first v first_value;
          match search assign_first with
          | Some _ as m -> m
          | None ->
              let assign_second = copy_assign assign in
              Hashtbl.add assign_second v second_value;
              search assign_second
  in
  search (Hashtbl.create 17)

let satisfiable st assumptions =
  match model st assumptions with
  | Some _ -> true
  | None -> false

let lit_true_in_model assign lit =
  match eval_lit assign lit with
  | Some true -> true
  | Some false | None -> false
