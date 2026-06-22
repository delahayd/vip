open Types

type result =
  | Satisfiable
  | Not_applicable of string
  | Too_large of string
  | Unsat

val run :
  ?max_instances:int ->
  ?check_timeout:(unit -> unit) ->
  clause list ->
  result
