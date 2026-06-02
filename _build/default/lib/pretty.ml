open Types
open Fof

let rec string_of_term = function
  | Var v -> v
  | Fun (f, []) -> f
  | Fun (f, args) ->
      Printf.sprintf "%s(%s)" f (String.concat "," (List.map string_of_term args))

let string_of_atom a =
  match a.args with
  | [] -> a.pred
  | xs -> Printf.sprintf "%s(%s)" a.pred (String.concat "," (List.map string_of_term xs))

let string_of_literal = function
  | Pos a -> string_of_atom a
  | Neg a -> "~" ^ string_of_atom a

let string_of_clause = function
  | [] -> "$false"
  | lits -> String.concat " | " (List.map string_of_literal lits)

let rec string_of_formula = function
  | FTrue -> "$true"
  | FFalse -> "$false"
  | Atom a -> string_of_atom a
  | Not f -> "~(" ^ string_of_formula f ^ ")"
  | And (a, b) -> "(" ^ string_of_formula a ^ " & " ^ string_of_formula b ^ ")"
  | Or (a, b) -> "(" ^ string_of_formula a ^ " | " ^ string_of_formula b ^ ")"
  | Imp (a, b) -> "(" ^ string_of_formula a ^ " => " ^ string_of_formula b ^ ")"
  | RevImp (a, b) -> "(" ^ string_of_formula a ^ " <= " ^ string_of_formula b ^ ")"
  | Iff (a, b) -> "(" ^ string_of_formula a ^ " <=> " ^ string_of_formula b ^ ")"
  | Xor (a, b) -> "(" ^ string_of_formula a ^ " <~> " ^ string_of_formula b ^ ")"
  | Forall (vs, f) -> "! [" ^ String.concat "," vs ^ "] : " ^ string_of_formula f
  | Exists (vs, f) -> "? [" ^ String.concat "," vs ^ "] : " ^ string_of_formula f
