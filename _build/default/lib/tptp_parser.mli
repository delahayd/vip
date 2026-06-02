
(* The type of tokens. *)

type token = 
  | XOR
  | VAR of (string)
  | TILDE
  | RPAREN
  | REVIMP
  | RBRACK
  | QUOTED of (string)
  | QMARK
  | NEQ
  | LPAREN
  | LBRACK
  | KW_TRUE
  | KW_INCLUDE
  | KW_FOF
  | KW_FALSE
  | KW_CNF
  | IMP
  | IFF
  | IDENT of (string)
  | EQUAL
  | EOF
  | DOT
  | COMMA
  | COLON
  | BAR
  | BANG
  | AMP

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fof.annotated_input list)
