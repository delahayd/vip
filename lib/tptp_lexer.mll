{
open Tptp_parser

exception Lexing_error of string

let keyword_or_ident s =
  match s with
  | "cnf" -> KW_CNF
  | "fof" -> KW_FOF
  | "include" -> KW_INCLUDE
  | "$false" -> KW_FALSE
  | "$true" -> KW_TRUE
  | _ -> IDENT s
}

let newline = '\n'
let blank = [' ' '\t' '\r']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let identchar = ['A'-'Z' 'a'-'z' '0'-'9' '_' '$']
let lower_ident = (lower | '$') identchar*
let upper_ident = (upper | '_') identchar*
let number_name = digit+

rule token = parse
  | blank+                    { token lexbuf }
  | newline                   { Lexing.new_line lexbuf; token lexbuf }
  | '%' [^ '\n']*             { token lexbuf }
  | "/*"                      { block_comment lexbuf; token lexbuf }
  | "<=>"                     { IFF }
  | "<~>"                     { XOR }
  | "=>"                      { IMP }
  | "<="                      { REVIMP }
  | "!="                      { NEQ }
  | '='                       { EQUAL }
  | '('                       { LPAREN }
  | ')'                       { RPAREN }
  | '['                       { LBRACK }
  | ']'                       { RBRACK }
  | ','                       { COMMA }
  | '.'                       { DOT }
  | ':'                       { COLON }
  | '|'                       { BAR }
  | '&'                       { AMP }
  | '~'                       { TILDE }
  | '!'                       { BANG }
  | '?'                       { QMARK }
  | '\''                      { quoted (Buffer.create 16) lexbuf }
  | upper_ident as s          { VAR s }
  | lower_ident as s          { keyword_or_ident s }
  | number_name as s          { IDENT s }
  | eof                       { EOF }
  | _ as c                    { raise (Lexing_error (Printf.sprintf "Caractère inattendu: %c" c)) }

and quoted buf = parse
  | "\\'"                     { Buffer.add_char buf '\''; quoted buf lexbuf }
  | "\\\\"                    { Buffer.add_char buf '\\'; quoted buf lexbuf }
  | '\''                      { QUOTED (Buffer.contents buf) }
  | newline                   { Buffer.add_char buf '\n'; Lexing.new_line lexbuf; quoted buf lexbuf }
  | eof                       { raise (Lexing_error "Chaîne quotée non terminée") }
  | _ as c                    { Buffer.add_char buf c; quoted buf lexbuf }

and block_comment = parse
  | "*/"                      { () }
  | newline                   { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof                       { raise (Lexing_error "Commentaire de bloc non terminé") }
  | _                         { block_comment lexbuf }
