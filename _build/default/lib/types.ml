module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

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

type substitution = term StringMap.t
