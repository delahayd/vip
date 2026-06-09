%{
open Types
open Fof
open Clause
%}

%token <string> IDENT
%token <string> VAR
%token <string> QUOTED
%token KW_CNF KW_FOF KW_INCLUDE
%token KW_FALSE KW_TRUE
%token LPAREN RPAREN LBRACK RBRACK COMMA DOT COLON
%token BAR AMP TILDE
%token IMP REVIMP IFF XOR
%token BANG QMARK
%token EQUAL NEQ
%token EOF

%start file
%type <Fof.annotated_input list> file
%type <(string * Types.term list)> symbol_app
%type <((string * Types.term list) -> Types.literal)> cnf_symbol_tail
%type <((string * Types.term list) -> Fof.formula)> fof_symbol_tail

%%

file:
  | entries EOF { List.rev $1 }

entries:
  | { [] }
  | entries top_entry { $2 :: $1 }

top_entry:
  | cnf_entry { $1 }
  | fof_entry { $1 }
  | include_entry { $1 }

include_entry:
  | KW_INCLUDE LPAREN QUOTED RPAREN DOT
      { Input_include { include_file = $3; include_only = None } }
  | KW_INCLUDE LPAREN QUOTED COMMA LBRACK name_list RBRACK RPAREN DOT
      { Input_include { include_file = $3; include_only = Some $6 } }

name_list:
  | ann_name { [$1] }
  | name_list COMMA ann_name { $1 @ [$3] }

cnf_entry:
  | KW_CNF LPAREN ann_name COMMA ann_role COMMA cnf_formula RPAREN DOT
      { Input_cnf { name = $3; role = $5; clause = normalize_clause $7 } }

fof_entry:
  | KW_FOF LPAREN ann_name COMMA ann_role COMMA formula RPAREN DOT
      { Input_fof { name = $3; role = $5; formula = $7 } }

atomic_word:
  | IDENT { $1 }
  | KW_INCLUDE { "include" }

ann_name:
  | atomic_word { $1 }
  | VAR { $1 }
  | QUOTED { $1 }

ann_role:
  | atomic_word { $1 }
  | VAR { $1 }
  | QUOTED { $1 }

var_list:
  | VAR { [$1] }
  | var_list COMMA VAR { $1 @ [$3] }

cnf_formula:
  | cnf_disjunction { $1 }
  | LPAREN cnf_formula RPAREN { $2 }
  | KW_FALSE { [] }
  | KW_TRUE { [ Pos { pred = "$true"; args = [] } ] }

cnf_disjunction:
  | cnf_literal { [$1] }
  | cnf_disjunction BAR cnf_literal { $1 @ [$3] }

cnf_literal:
  | sa = symbol_app; tail = cnf_symbol_tail { tail sa }
  | TILDE sa = symbol_app
      {
        let (p, args) = sa in
        Neg { pred = p; args = args }
      }
  | VAR EQUAL term
      { Pos { pred = "="; args = [Var $1; $3] } }
  | VAR NEQ term
      { Neg { pred = "="; args = [Var $1; $3] } }
  | KW_TRUE
      { Pos { pred = "$true"; args = [] } }
  | KW_FALSE
      { Pos { pred = "$false"; args = [] } }

cnf_symbol_tail:
  | EQUAL term
      {
        fun (f, args) ->
          Pos { pred = "="; args = [Fun (f, args); $2] }
      }
  | NEQ term
      {
        fun (f, args) ->
          Neg { pred = "="; args = [Fun (f, args); $2] }
      }
  |
      {
        fun (p, args) ->
          Pos { pred = p; args = args }
      }

formula:
  | equiv_formula { $1 }

equiv_formula:
  | impl_formula { $1 }
  | impl_formula IFF equiv_formula { Iff ($1, $3) }
  | impl_formula XOR equiv_formula { Xor ($1, $3) }

impl_formula:
  | or_formula { $1 }
  | or_formula IMP impl_formula { Imp ($1, $3) }
  | or_formula REVIMP impl_formula { RevImp ($1, $3) }

or_formula:
  | and_formula { $1 }
  | or_formula BAR and_formula { Or ($1, $3) }

and_formula:
  | unary_formula { $1 }
  | and_formula AMP unary_formula { And ($1, $3) }

unary_formula:
  | atomic_formula { $1 }
  | quantified_formula { $1 }
  | TILDE unary_formula { Not $2 }
  | LPAREN formula RPAREN { $2 }

quantified_formula:
  | BANG LBRACK var_list RBRACK COLON unary_formula { Forall ($3, $6) }
  | QMARK LBRACK var_list RBRACK COLON unary_formula { Exists ($3, $6) }

atomic_formula:
  | sa = symbol_app; tail = fof_symbol_tail { tail sa }
  | VAR EQUAL term
      { Atom { pred = "="; args = [Var $1; $3] } }
  | VAR NEQ term
      { Not (Atom { pred = "="; args = [Var $1; $3] }) }
  | KW_TRUE { FTrue }
  | KW_FALSE { FFalse }

fof_symbol_tail:
  | EQUAL term
      {
        fun (f, args) ->
          Atom { pred = "="; args = [Fun (f, args); $2] }
      }
  | NEQ term
      {
        fun (f, args) ->
          Not (Atom { pred = "="; args = [Fun (f, args); $2] })
      }
  |
      {
        fun (p, args) ->
          Atom { pred = p; args = args }
      }

symbol_app:
  | atomic_word { ($1, []) }
  | atomic_word LPAREN term_list RPAREN { ($1, $3) }
  | QUOTED { ($1, []) }
  | QUOTED LPAREN term_list RPAREN { ($1, $3) }

term_list:
  | term { [$1] }
  | term_list COMMA term { $1 @ [$3] }

term:
  | VAR { Var $1 }
  | symbol_app
      {
        let (f, args) = $1 in
        Fun (f, args)
      }
