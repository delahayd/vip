open Fof

exception Error of string

type parsed_problem = {
  file : string option;
  inputs : annotated_input list;
}

type load_config = {
  include_paths : string list;
  use_tptp_env : bool;
}

val default_load_config : load_config

val parse_string : string -> annotated_input list
val parse_channel : in_channel -> annotated_input list
val parse_file : string -> annotated_input list

val load_problem : ?config:load_config -> string -> parsed_problem
