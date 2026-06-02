type term =
  | Var of string
  | Fun of string * term list

type atom = {
  pred : string;
  args : term list;
}

type literal =
  | Pos of atom
  | Neg of atom

type clause = literal list

module StringMap : Map.S with type key = string
module StringSet : Set.S with type elt = string

type substitution = term StringMap.t
