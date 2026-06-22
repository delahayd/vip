type lit =
  | Pos of int
  | Neg of int

type t

val create : unit -> t
val new_var : t -> int
val add_clause : t -> lit list -> unit
val model : ?prefer_false:bool -> t -> lit list -> (int, bool) Hashtbl.t option
val satisfiable : t -> lit list -> bool
val lit_true_in_model : (int, bool) Hashtbl.t -> lit -> bool
