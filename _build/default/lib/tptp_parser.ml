
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | XOR
    | VAR of 
# 8 "lib/tptp_parser.mly"
       (string)
# 16 "lib/tptp_parser.ml"
  
    | TILDE
    | RPAREN
    | REVIMP
    | RBRACK
    | QUOTED of 
# 9 "lib/tptp_parser.mly"
       (string)
# 25 "lib/tptp_parser.ml"
  
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
    | IDENT of 
# 7 "lib/tptp_parser.mly"
       (string)
# 41 "lib/tptp_parser.ml"
  
    | EQUAL
    | EOF
    | DOT
    | COMMA
    | COLON
    | BAR
    | BANG
    | AMP
  
end

include MenhirBasics

# 1 "lib/tptp_parser.mly"
  
open Types
open Fof
open Clause

# 62 "lib/tptp_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState009 : ('s _menhir_cell0_entries _menhir_cell0_QUOTED, _menhir_box_file) _menhir_state
    (** State 009.
        Stack shape : entries QUOTED.
        Start symbol: file. *)

  | MenhirState017 : (('s _menhir_cell0_entries _menhir_cell0_QUOTED, _menhir_box_file) _menhir_cell1_name_list, _menhir_box_file) _menhir_state
    (** State 017.
        Stack shape : entries QUOTED name_list.
        Start symbol: file. *)

  | MenhirState021 : ('s _menhir_cell0_entries, _menhir_box_file) _menhir_state
    (** State 021.
        Stack shape : entries.
        Start symbol: file. *)

  | MenhirState023 : (('s _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name, _menhir_box_file) _menhir_state
    (** State 023.
        Stack shape : entries ann_name.
        Start symbol: file. *)

  | MenhirState028 : ((('s _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name, _menhir_box_file) _menhir_cell1_ann_role, _menhir_box_file) _menhir_state
    (** State 028.
        Stack shape : entries ann_name ann_role.
        Start symbol: file. *)

  | MenhirState030 : (('s, _menhir_box_file) _menhir_cell1_VAR, _menhir_box_file) _menhir_state
    (** State 030.
        Stack shape : VAR.
        Start symbol: file. *)

  | MenhirState033 : (('s, _menhir_box_file) _menhir_cell1_QUOTED, _menhir_box_file) _menhir_state
    (** State 033.
        Stack shape : QUOTED.
        Start symbol: file. *)

  | MenhirState035 : (('s, _menhir_box_file) _menhir_cell1_IDENT, _menhir_box_file) _menhir_state
    (** State 035.
        Stack shape : IDENT.
        Start symbol: file. *)

  | MenhirState038 : (('s, _menhir_box_file) _menhir_cell1_term_list, _menhir_box_file) _menhir_state
    (** State 038.
        Stack shape : term_list.
        Start symbol: file. *)

  | MenhirState045 : (('s, _menhir_box_file) _menhir_cell1_VAR, _menhir_box_file) _menhir_state
    (** State 045.
        Stack shape : VAR.
        Start symbol: file. *)

  | MenhirState047 : (('s, _menhir_box_file) _menhir_cell1_TILDE, _menhir_box_file) _menhir_state
    (** State 047.
        Stack shape : TILDE.
        Start symbol: file. *)

  | MenhirState049 : (('s, _menhir_box_file) _menhir_cell1_QMARK, _menhir_box_file) _menhir_state
    (** State 049.
        Stack shape : QMARK.
        Start symbol: file. *)

  | MenhirState053 : ((('s, _menhir_box_file) _menhir_cell1_QMARK, _menhir_box_file) _menhir_cell1_var_list, _menhir_box_file) _menhir_state
    (** State 053.
        Stack shape : QMARK var_list.
        Start symbol: file. *)

  | MenhirState054 : (('s, _menhir_box_file) _menhir_cell1_LPAREN, _menhir_box_file) _menhir_state
    (** State 054.
        Stack shape : LPAREN.
        Start symbol: file. *)

  | MenhirState058 : (('s, _menhir_box_file) _menhir_cell1_BANG, _menhir_box_file) _menhir_state
    (** State 058.
        Stack shape : BANG.
        Start symbol: file. *)

  | MenhirState061 : ((('s, _menhir_box_file) _menhir_cell1_BANG, _menhir_box_file) _menhir_cell1_var_list, _menhir_box_file) _menhir_state
    (** State 061.
        Stack shape : BANG var_list.
        Start symbol: file. *)

  | MenhirState064 : (('s, _menhir_box_file) _menhir_cell1_symbol_app, _menhir_box_file) _menhir_state
    (** State 064.
        Stack shape : symbol_app.
        Start symbol: file. *)

  | MenhirState066 : (('s, _menhir_box_file) _menhir_cell1_symbol_app, _menhir_box_file) _menhir_state
    (** State 066.
        Stack shape : symbol_app.
        Start symbol: file. *)

  | MenhirState075 : (('s, _menhir_box_file) _menhir_cell1_or_formula, _menhir_box_file) _menhir_state
    (** State 075.
        Stack shape : or_formula.
        Start symbol: file. *)

  | MenhirState078 : (('s, _menhir_box_file) _menhir_cell1_and_formula, _menhir_box_file) _menhir_state
    (** State 078.
        Stack shape : and_formula.
        Start symbol: file. *)

  | MenhirState080 : (('s, _menhir_box_file) _menhir_cell1_or_formula, _menhir_box_file) _menhir_state
    (** State 080.
        Stack shape : or_formula.
        Start symbol: file. *)

  | MenhirState082 : (('s, _menhir_box_file) _menhir_cell1_or_formula, _menhir_box_file) _menhir_state
    (** State 082.
        Stack shape : or_formula.
        Start symbol: file. *)

  | MenhirState085 : (('s, _menhir_box_file) _menhir_cell1_impl_formula, _menhir_box_file) _menhir_state
    (** State 085.
        Stack shape : impl_formula.
        Start symbol: file. *)

  | MenhirState087 : (('s, _menhir_box_file) _menhir_cell1_impl_formula, _menhir_box_file) _menhir_state
    (** State 087.
        Stack shape : impl_formula.
        Start symbol: file. *)

  | MenhirState098 : ('s _menhir_cell0_entries, _menhir_box_file) _menhir_state
    (** State 098.
        Stack shape : entries.
        Start symbol: file. *)

  | MenhirState100 : (('s _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name, _menhir_box_file) _menhir_state
    (** State 100.
        Stack shape : entries ann_name.
        Start symbol: file. *)

  | MenhirState102 : ((('s _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name, _menhir_box_file) _menhir_cell1_ann_role, _menhir_box_file) _menhir_state
    (** State 102.
        Stack shape : entries ann_name ann_role.
        Start symbol: file. *)

  | MenhirState104 : (('s, _menhir_box_file) _menhir_cell1_VAR, _menhir_box_file) _menhir_state
    (** State 104.
        Stack shape : VAR.
        Start symbol: file. *)

  | MenhirState106 : (('s, _menhir_box_file) _menhir_cell1_VAR, _menhir_box_file) _menhir_state
    (** State 106.
        Stack shape : VAR.
        Start symbol: file. *)

  | MenhirState108 : (('s, _menhir_box_file) _menhir_cell1_TILDE, _menhir_box_file) _menhir_state
    (** State 108.
        Stack shape : TILDE.
        Start symbol: file. *)

  | MenhirState110 : (('s, _menhir_box_file) _menhir_cell1_LPAREN, _menhir_box_file) _menhir_state
    (** State 110.
        Stack shape : LPAREN.
        Start symbol: file. *)

  | MenhirState114 : (('s, _menhir_box_file) _menhir_cell1_symbol_app, _menhir_box_file) _menhir_state
    (** State 114.
        Stack shape : symbol_app.
        Start symbol: file. *)

  | MenhirState116 : (('s, _menhir_box_file) _menhir_cell1_symbol_app, _menhir_box_file) _menhir_state
    (** State 116.
        Stack shape : symbol_app.
        Start symbol: file. *)

  | MenhirState123 : (('s, _menhir_box_file) _menhir_cell1_cnf_disjunction, _menhir_box_file) _menhir_state
    (** State 123.
        Stack shape : cnf_disjunction.
        Start symbol: file. *)


and ('s, 'r) _menhir_cell1_and_formula = 
  | MenhirCell1_and_formula of 's * ('s, 'r) _menhir_state * (Fof.formula)

and ('s, 'r) _menhir_cell1_ann_name = 
  | MenhirCell1_ann_name of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_ann_role = 
  | MenhirCell1_ann_role of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_cnf_disjunction = 
  | MenhirCell1_cnf_disjunction of 's * ('s, 'r) _menhir_state * (Types.clause)

and 's _menhir_cell0_entries = 
  | MenhirCell0_entries of 's * (Fof.annotated_input list)

and ('s, 'r) _menhir_cell1_impl_formula = 
  | MenhirCell1_impl_formula of 's * ('s, 'r) _menhir_state * (Fof.formula)

and ('s, 'r) _menhir_cell1_name_list = 
  | MenhirCell1_name_list of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_or_formula = 
  | MenhirCell1_or_formula of 's * ('s, 'r) _menhir_state * (Fof.formula)

and ('s, 'r) _menhir_cell1_symbol_app = 
  | MenhirCell1_symbol_app of 's * ('s, 'r) _menhir_state * (string * Types.term list)

and ('s, 'r) _menhir_cell1_term_list = 
  | MenhirCell1_term_list of 's * ('s, 'r) _menhir_state * (Types.term list)

and ('s, 'r) _menhir_cell1_var_list = 
  | MenhirCell1_var_list of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_BANG = 
  | MenhirCell1_BANG of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * 
# 7 "lib/tptp_parser.mly"
       (string)
# 276 "lib/tptp_parser.ml"


and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_QMARK = 
  | MenhirCell1_QMARK of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_QUOTED = 
  | MenhirCell1_QUOTED of 's * ('s, 'r) _menhir_state * 
# 9 "lib/tptp_parser.mly"
       (string)
# 289 "lib/tptp_parser.ml"


and 's _menhir_cell0_QUOTED = 
  | MenhirCell0_QUOTED of 's * 
# 9 "lib/tptp_parser.mly"
       (string)
# 296 "lib/tptp_parser.ml"


and ('s, 'r) _menhir_cell1_TILDE = 
  | MenhirCell1_TILDE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * 
# 8 "lib/tptp_parser.mly"
       (string)
# 306 "lib/tptp_parser.ml"


and _menhir_box_file = 
  | MenhirBox_file of (Fof.annotated_input list) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (
# 132 "lib/tptp_parser.mly"
                  ( _1 )
# 317 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_02 =
  fun _1 _3 ->
    (
# 133 "lib/tptp_parser.mly"
                                  ( And (_1, _3) )
# 325 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_03 =
  fun _1 ->
    (
# 58 "lib/tptp_parser.mly"
          ( _1 )
# 333 "lib/tptp_parser.ml"
     : (string))

let _menhir_action_04 =
  fun _1 ->
    (
# 59 "lib/tptp_parser.mly"
        ( _1 )
# 341 "lib/tptp_parser.ml"
     : (string))

let _menhir_action_05 =
  fun _1 ->
    (
# 60 "lib/tptp_parser.mly"
           ( _1 )
# 349 "lib/tptp_parser.ml"
     : (string))

let _menhir_action_06 =
  fun _1 ->
    (
# 63 "lib/tptp_parser.mly"
          ( _1 )
# 357 "lib/tptp_parser.ml"
     : (string))

let _menhir_action_07 =
  fun _1 ->
    (
# 64 "lib/tptp_parser.mly"
        ( _1 )
# 365 "lib/tptp_parser.ml"
     : (string))

let _menhir_action_08 =
  fun _1 ->
    (
# 65 "lib/tptp_parser.mly"
           ( _1 )
# 373 "lib/tptp_parser.ml"
     : (string))

let _menhir_action_09 =
  fun sa tail ->
    (
# 146 "lib/tptp_parser.mly"
                                            ( tail sa )
# 381 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_10 =
  fun _1 _3 ->
    (
# 148 "lib/tptp_parser.mly"
      ( Atom { pred = "="; args = [Var _1; _3] } )
# 389 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_11 =
  fun _1 _3 ->
    (
# 150 "lib/tptp_parser.mly"
      ( Not (Atom { pred = "="; args = [Var _1; _3] }) )
# 397 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_12 =
  fun () ->
    (
# 151 "lib/tptp_parser.mly"
            ( FTrue )
# 405 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_13 =
  fun () ->
    (
# 152 "lib/tptp_parser.mly"
             ( FFalse )
# 413 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_14 =
  fun _1 ->
    (
# 78 "lib/tptp_parser.mly"
                ( [_1] )
# 421 "lib/tptp_parser.ml"
     : (Types.clause))

let _menhir_action_15 =
  fun _1 _3 ->
    (
# 79 "lib/tptp_parser.mly"
                                    ( _1 @ [_3] )
# 429 "lib/tptp_parser.ml"
     : (Types.clause))

let _menhir_action_16 =
  fun _3 _5 _7 ->
    (
# 51 "lib/tptp_parser.mly"
      ( Input_cnf { name = _3; role = _5; clause = normalize_clause _7 } )
# 437 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_17 =
  fun _1 ->
    (
# 72 "lib/tptp_parser.mly"
                    ( _1 )
# 445 "lib/tptp_parser.ml"
     : (Types.clause))

let _menhir_action_18 =
  fun _2 ->
    (
# 73 "lib/tptp_parser.mly"
                              ( _2 )
# 453 "lib/tptp_parser.ml"
     : (Types.clause))

let _menhir_action_19 =
  fun () ->
    (
# 74 "lib/tptp_parser.mly"
             ( [] )
# 461 "lib/tptp_parser.ml"
     : (Types.clause))

let _menhir_action_20 =
  fun () ->
    (
# 75 "lib/tptp_parser.mly"
            ( [ Pos { pred = "$true"; args = [] } ] )
# 469 "lib/tptp_parser.ml"
     : (Types.clause))

let _menhir_action_21 =
  fun sa tail ->
    (
# 82 "lib/tptp_parser.mly"
                                            ( tail sa )
# 477 "lib/tptp_parser.ml"
     : (Types.literal))

let _menhir_action_22 =
  fun sa ->
    (
# 84 "lib/tptp_parser.mly"
      (
        let (p, args) = sa in
        Neg { pred = p; args = args }
      )
# 488 "lib/tptp_parser.ml"
     : (Types.literal))

let _menhir_action_23 =
  fun _1 _3 ->
    (
# 89 "lib/tptp_parser.mly"
      ( Pos { pred = "="; args = [Var _1; _3] } )
# 496 "lib/tptp_parser.ml"
     : (Types.literal))

let _menhir_action_24 =
  fun _1 _3 ->
    (
# 91 "lib/tptp_parser.mly"
      ( Neg { pred = "="; args = [Var _1; _3] } )
# 504 "lib/tptp_parser.ml"
     : (Types.literal))

let _menhir_action_25 =
  fun () ->
    (
# 93 "lib/tptp_parser.mly"
      ( Pos { pred = "$true"; args = [] } )
# 512 "lib/tptp_parser.ml"
     : (Types.literal))

let _menhir_action_26 =
  fun () ->
    (
# 95 "lib/tptp_parser.mly"
      ( Pos { pred = "$false"; args = [] } )
# 520 "lib/tptp_parser.ml"
     : (Types.literal))

let _menhir_action_27 =
  fun _2 ->
    (
# 99 "lib/tptp_parser.mly"
      (
        fun (f, args) ->
          Pos { pred = "="; args = [Fun (f, args); _2] }
      )
# 531 "lib/tptp_parser.ml"
     : (string * Types.term list -> Types.literal))

let _menhir_action_28 =
  fun _2 ->
    (
# 104 "lib/tptp_parser.mly"
      (
        fun (f, args) ->
          Neg { pred = "="; args = [Fun (f, args); _2] }
      )
# 542 "lib/tptp_parser.ml"
     : (string * Types.term list -> Types.literal))

let _menhir_action_29 =
  fun () ->
    (
# 109 "lib/tptp_parser.mly"
      (
        fun (p, args) ->
          Pos { pred = p; args = args }
      )
# 553 "lib/tptp_parser.ml"
     : (string * Types.term list -> Types.literal))

let _menhir_action_30 =
  fun () ->
    (
# 31 "lib/tptp_parser.mly"
    ( [] )
# 561 "lib/tptp_parser.ml"
     : (Fof.annotated_input list))

let _menhir_action_31 =
  fun _1 _2 ->
    (
# 32 "lib/tptp_parser.mly"
                      ( _2 :: _1 )
# 569 "lib/tptp_parser.ml"
     : (Fof.annotated_input list))

let _menhir_action_32 =
  fun _1 ->
    (
# 118 "lib/tptp_parser.mly"
                 ( _1 )
# 577 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_33 =
  fun _1 _3 ->
    (
# 119 "lib/tptp_parser.mly"
                                   ( Iff (_1, _3) )
# 585 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_34 =
  fun _1 _3 ->
    (
# 120 "lib/tptp_parser.mly"
                                   ( Xor (_1, _3) )
# 593 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_35 =
  fun _1 ->
    (
# 28 "lib/tptp_parser.mly"
                ( List.rev _1 )
# 601 "lib/tptp_parser.ml"
     : (Fof.annotated_input list))

let _menhir_action_36 =
  fun _3 _5 _7 ->
    (
# 55 "lib/tptp_parser.mly"
      ( Input_fof { name = _3; role = _5; formula = _7 } )
# 609 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_37 =
  fun _2 ->
    (
# 156 "lib/tptp_parser.mly"
      (
        fun (f, args) ->
          Atom { pred = "="; args = [Fun (f, args); _2] }
      )
# 620 "lib/tptp_parser.ml"
     : (string * Types.term list -> Fof.formula))

let _menhir_action_38 =
  fun _2 ->
    (
# 161 "lib/tptp_parser.mly"
      (
        fun (f, args) ->
          Not (Atom { pred = "="; args = [Fun (f, args); _2] })
      )
# 631 "lib/tptp_parser.ml"
     : (string * Types.term list -> Fof.formula))

let _menhir_action_39 =
  fun () ->
    (
# 166 "lib/tptp_parser.mly"
      (
        fun (p, args) ->
          Atom { pred = p; args = args }
      )
# 642 "lib/tptp_parser.ml"
     : (string * Types.term list -> Fof.formula))

let _menhir_action_40 =
  fun _1 ->
    (
# 115 "lib/tptp_parser.mly"
                  ( _1 )
# 650 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_41 =
  fun _1 ->
    (
# 123 "lib/tptp_parser.mly"
               ( _1 )
# 658 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_42 =
  fun _1 _3 ->
    (
# 124 "lib/tptp_parser.mly"
                                ( Imp (_1, _3) )
# 666 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_43 =
  fun _1 _3 ->
    (
# 125 "lib/tptp_parser.mly"
                                   ( RevImp (_1, _3) )
# 674 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_44 =
  fun _3 ->
    (
# 41 "lib/tptp_parser.mly"
      ( Input_include { include_file = _3; include_only = None } )
# 682 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_45 =
  fun _3 _6 ->
    (
# 43 "lib/tptp_parser.mly"
      ( Input_include { include_file = _3; include_only = Some _6 } )
# 690 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_46 =
  fun _1 ->
    (
# 46 "lib/tptp_parser.mly"
             ( [_1] )
# 698 "lib/tptp_parser.ml"
     : (string list))

let _menhir_action_47 =
  fun _1 _3 ->
    (
# 47 "lib/tptp_parser.mly"
                             ( _1 @ [_3] )
# 706 "lib/tptp_parser.ml"
     : (string list))

let _menhir_action_48 =
  fun _1 ->
    (
# 128 "lib/tptp_parser.mly"
                ( _1 )
# 714 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_49 =
  fun _1 _3 ->
    (
# 129 "lib/tptp_parser.mly"
                               ( Or (_1, _3) )
# 722 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_50 =
  fun _3 _6 ->
    (
# 142 "lib/tptp_parser.mly"
                                                    ( Forall (_3, _6) )
# 730 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_51 =
  fun _3 _6 ->
    (
# 143 "lib/tptp_parser.mly"
                                                     ( Exists (_3, _6) )
# 738 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_52 =
  fun _1 ->
    (
# 172 "lib/tptp_parser.mly"
          ( (_1, []) )
# 746 "lib/tptp_parser.ml"
     : (string * Types.term list))

let _menhir_action_53 =
  fun _1 _3 ->
    (
# 173 "lib/tptp_parser.mly"
                                  ( (_1, _3) )
# 754 "lib/tptp_parser.ml"
     : (string * Types.term list))

let _menhir_action_54 =
  fun _1 ->
    (
# 174 "lib/tptp_parser.mly"
           ( (_1, []) )
# 762 "lib/tptp_parser.ml"
     : (string * Types.term list))

let _menhir_action_55 =
  fun _1 _3 ->
    (
# 175 "lib/tptp_parser.mly"
                                   ( (_1, _3) )
# 770 "lib/tptp_parser.ml"
     : (string * Types.term list))

let _menhir_action_56 =
  fun _1 ->
    (
# 182 "lib/tptp_parser.mly"
        ( Var _1 )
# 778 "lib/tptp_parser.ml"
     : (Types.term))

let _menhir_action_57 =
  fun _1 ->
    (
# 184 "lib/tptp_parser.mly"
      (
        let (f, args) = _1 in
        Fun (f, args)
      )
# 789 "lib/tptp_parser.ml"
     : (Types.term))

let _menhir_action_58 =
  fun _1 ->
    (
# 178 "lib/tptp_parser.mly"
         ( [_1] )
# 797 "lib/tptp_parser.ml"
     : (Types.term list))

let _menhir_action_59 =
  fun _1 _3 ->
    (
# 179 "lib/tptp_parser.mly"
                         ( _1 @ [_3] )
# 805 "lib/tptp_parser.ml"
     : (Types.term list))

let _menhir_action_60 =
  fun _1 ->
    (
# 35 "lib/tptp_parser.mly"
              ( _1 )
# 813 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_61 =
  fun _1 ->
    (
# 36 "lib/tptp_parser.mly"
              ( _1 )
# 821 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_62 =
  fun _1 ->
    (
# 37 "lib/tptp_parser.mly"
                  ( _1 )
# 829 "lib/tptp_parser.ml"
     : (Fof.annotated_input))

let _menhir_action_63 =
  fun _1 ->
    (
# 136 "lib/tptp_parser.mly"
                   ( _1 )
# 837 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_64 =
  fun _1 ->
    (
# 137 "lib/tptp_parser.mly"
                       ( _1 )
# 845 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_65 =
  fun _2 ->
    (
# 138 "lib/tptp_parser.mly"
                        ( Not _2 )
# 853 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_66 =
  fun _2 ->
    (
# 139 "lib/tptp_parser.mly"
                          ( _2 )
# 861 "lib/tptp_parser.ml"
     : (Fof.formula))

let _menhir_action_67 =
  fun _1 ->
    (
# 68 "lib/tptp_parser.mly"
        ( [_1] )
# 869 "lib/tptp_parser.ml"
     : (string list))

let _menhir_action_68 =
  fun _1 _3 ->
    (
# 69 "lib/tptp_parser.mly"
                       ( _1 @ [_3] )
# 877 "lib/tptp_parser.ml"
     : (string list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | XOR ->
        "XOR"
    | VAR _ ->
        "VAR"
    | TILDE ->
        "TILDE"
    | RPAREN ->
        "RPAREN"
    | REVIMP ->
        "REVIMP"
    | RBRACK ->
        "RBRACK"
    | QUOTED _ ->
        "QUOTED"
    | QMARK ->
        "QMARK"
    | NEQ ->
        "NEQ"
    | LPAREN ->
        "LPAREN"
    | LBRACK ->
        "LBRACK"
    | KW_TRUE ->
        "KW_TRUE"
    | KW_INCLUDE ->
        "KW_INCLUDE"
    | KW_FOF ->
        "KW_FOF"
    | KW_FALSE ->
        "KW_FALSE"
    | KW_CNF ->
        "KW_CNF"
    | IMP ->
        "IMP"
    | IFF ->
        "IFF"
    | IDENT _ ->
        "IDENT"
    | EQUAL ->
        "EQUAL"
    | EOF ->
        "EOF"
    | DOT ->
        "DOT"
    | COMMA ->
        "COMMA"
    | COLON ->
        "COLON"
    | BAR ->
        "BAR"
    | BANG ->
        "BANG"
    | AMP ->
        "AMP"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_goto_entries : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | KW_INCLUDE ->
          let _menhir_stack = MenhirCell0_entries (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | QUOTED _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | RPAREN ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | DOT ->
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          let _3 = _v_0 in
                          let _v = _menhir_action_44 _3 in
                          _menhir_goto_include_entry _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
                      | _ ->
                          _eRR ())
                  | COMMA ->
                      let _menhir_stack = MenhirCell0_QUOTED (_menhir_stack, _v_0) in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | LBRACK ->
                          let _menhir_s = MenhirState009 in
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | VAR _v ->
                              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | QUOTED _v ->
                              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | IDENT _v ->
                              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | KW_FOF ->
          let _menhir_stack = MenhirCell0_entries (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState021 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | QUOTED _v ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | KW_CNF ->
          let _menhir_stack = MenhirCell0_entries (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState098 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | QUOTED _v ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_35 _1 in
          MenhirBox_file _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_include_entry : type  ttv_stack. ttv_stack _menhir_cell0_entries -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_62 _1 in
      _menhir_goto_top_entry _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_top_entry : type  ttv_stack. ttv_stack _menhir_cell0_entries -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_entries (_menhir_stack, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_31 _1 _2 in
      _menhir_goto_entries _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_04 _1 in
      _menhir_goto_ann_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ann_name : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState009 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_018 : type  ttv_stack. (ttv_stack _menhir_cell0_entries _menhir_cell0_QUOTED, _menhir_box_file) _menhir_cell1_name_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_name_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_47 _1 _3 in
      _menhir_goto_name_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_name_list : type  ttv_stack. (ttv_stack _menhir_cell0_entries _menhir_cell0_QUOTED as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | DOT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell0_QUOTED (_menhir_stack, _3) = _menhir_stack in
                  let _6 = _v in
                  let _v = _menhir_action_45 _3 _6 in
                  _menhir_goto_include_entry _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA ->
          let _menhir_stack = MenhirCell1_name_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState017 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_05 _1 in
      _menhir_goto_ann_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_03 _1 in
      _menhir_goto_ann_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_019 : type  ttv_stack. (ttv_stack _menhir_cell0_entries _menhir_cell0_QUOTED as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_46 _1 in
      _menhir_goto_name_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_022 : type  ttv_stack. (ttv_stack _menhir_cell0_entries as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ann_name (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState023 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ann_name as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_07 _1 in
      _menhir_goto_ann_role _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ann_role : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ann_name as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState023 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_027 : type  ttv_stack. ((ttv_stack _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ann_role (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState028 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QMARK ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NEQ ->
          let _menhir_s = MenhirState030 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_s = MenhirState045 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_56 _1 in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_term : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState038 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState045 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState066 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState106 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_039 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_term_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_term_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_59 _1 _3 in
      _menhir_goto_term_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_term_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState035 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_036 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_53 _1 _3 in
          _menhir_goto_symbol_app _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_term_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_symbol_app : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState030 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState102 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_040 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_57 _1 in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_063 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_symbol_app (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | NEQ ->
          let _menhir_s = MenhirState064 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_s = MenhirState066 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | BAR | IFF | IMP | REVIMP | RPAREN | XOR ->
          let _v = _menhir_action_39 () in
          _menhir_goto_fof_symbol_tail _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_QUOTED (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState033 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | BAR | COMMA | EQUAL | IFF | IMP | NEQ | REVIMP | RPAREN | XOR ->
          let _1 = _v in
          let _v = _menhir_action_54 _1 in
          _menhir_goto_symbol_app _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState035 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | AMP | BAR | COMMA | EQUAL | IFF | IMP | NEQ | REVIMP | RPAREN | XOR ->
          let _1 = _v in
          let _v = _menhir_action_52 _1 in
          _menhir_goto_symbol_app _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_fof_symbol_tail : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_symbol_app -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_symbol_app (_menhir_stack, _menhir_s, sa) = _menhir_stack in
      let tail = _v in
      let _v = _menhir_action_09 sa tail in
      _menhir_goto_atomic_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_atomic_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_63 _1 in
      _menhir_goto_unary_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState028 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState053 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState047 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_062 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_BANG, _menhir_box_file) _menhir_cell1_var_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_var_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_BANG (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_50 _3 _6 in
      _menhir_goto_quantified_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_quantified_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_64 _1 in
      _menhir_goto_unary_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_goto_and_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_and_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState028 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AMP ->
          let _menhir_stack = MenhirCell1_and_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | IFF | IMP | REVIMP | RPAREN | XOR ->
          let _1 = _v in
          let _v = _menhir_action_48 _1 in
          _menhir_goto_or_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_and_formula -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState078 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TILDE ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | QUOTED _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | QMARK ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_TRUE ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_FALSE ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TILDE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState047 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TILDE ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | QUOTED _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | QMARK ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_TRUE ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_FALSE ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_QMARK (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACK ->
          let _menhir_s = MenhirState049 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_67 _1 in
      _menhir_goto_var_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_var_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState049 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_051 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_QMARK as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_var_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RBRACK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_s = MenhirState053 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TILDE ->
                  _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | QUOTED _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | QMARK ->
                  _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | KW_TRUE ->
                  _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | KW_FALSE ->
                  _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState054 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TILDE ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | QUOTED _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | QMARK ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_TRUE ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_FALSE ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_atomic_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_056 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_13 () in
      _menhir_goto_atomic_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_057 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BANG (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACK ->
          let _menhir_s = MenhirState058 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_var_list -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_var_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_68 _1 _3 in
          _menhir_goto_var_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_BANG as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_var_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RBRACK ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_s = MenhirState061 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | VAR _v ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | TILDE ->
                  _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | QUOTED _v ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | QMARK ->
                  _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | KW_TRUE ->
                  _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | KW_FALSE ->
                  _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_or_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | REVIMP ->
          let _menhir_stack = MenhirCell1_or_formula (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState075 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QMARK ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | IMP ->
          let _menhir_stack = MenhirCell1_or_formula (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState080 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QMARK ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | BAR ->
          let _menhir_stack = MenhirCell1_or_formula (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState082 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QMARK ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | IFF | RPAREN | XOR ->
          let _1 = _v in
          let _v = _menhir_action_41 _1 in
          _menhir_goto_impl_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_impl_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState080 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState028 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_076 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_or_formula -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_or_formula (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_43 _1 _3 in
      _menhir_goto_impl_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_or_formula -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_or_formula (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_42 _1 _3 in
      _menhir_goto_impl_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_impl_formula (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState085 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QMARK ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | IFF ->
          let _menhir_stack = MenhirCell1_impl_formula (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState087 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QMARK ->
              _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_32 _1 in
          _menhir_goto_equiv_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_equiv_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState028 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState054 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_impl_formula -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_impl_formula (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_34 _1 _3 in
      _menhir_goto_equiv_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_impl_formula -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_impl_formula (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_33 _1 _3 in
      _menhir_goto_equiv_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _1 = _v in
      let _v = _menhir_action_40 _1 in
      _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState054 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState028 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_089 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_66 _2 in
      _menhir_goto_unary_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_094 : type  ttv_stack. ((ttv_stack _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name, _menhir_box_file) _menhir_cell1_ann_role -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ann_role (_menhir_stack, _, _5) = _menhir_stack in
          let MenhirCell1_ann_name (_menhir_stack, _, _3) = _menhir_stack in
          let _7 = _v in
          let _v = _menhir_action_36 _3 _5 _7 in
          let _1 = _v in
          let _v = _menhir_action_61 _1 in
          _menhir_goto_top_entry _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_or_formula as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AMP ->
          let _menhir_stack = MenhirCell1_and_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BAR | IFF | IMP | REVIMP | RPAREN | XOR ->
          let MenhirCell1_or_formula (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_49 _1 _3 in
          _menhir_goto_or_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_and_formula -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_and_formula (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_02 _1 _3 in
      _menhir_goto_and_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_QMARK, _menhir_box_file) _menhir_cell1_var_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_var_list (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_QMARK (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_51 _3 _6 in
      _menhir_goto_quantified_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_093 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_TILDE -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TILDE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_65 _2 in
      _menhir_goto_unary_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_109 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_TILDE -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TILDE (_menhir_stack, _menhir_s) = _menhir_stack in
      let sa = _v in
      let _v = _menhir_action_22 sa in
      _menhir_goto_cnf_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_cnf_literal : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState102 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_119 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_14 _1 in
      _menhir_goto_cnf_disjunction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_cnf_disjunction : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_cnf_disjunction (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState123
          | TILDE ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
          | QUOTED _v_1 ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState123
          | KW_TRUE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_25 () in
              _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
          | KW_FALSE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_3 = _menhir_action_26 () in
              _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 _tok
          | IDENT _v_4 ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState123
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_17 _1 in
          _menhir_goto_cnf_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_103 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | NEQ ->
          let _menhir_s = MenhirState104 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_s = MenhirState106 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TILDE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState108 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | QUOTED _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_cnf_disjunction -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_cnf_disjunction (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_15 _1 _3 in
      _menhir_goto_cnf_disjunction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_cnf_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState110 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState102 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_120 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_18 _2 in
          _menhir_goto_cnf_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_127 : type  ttv_stack. ((ttv_stack _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name, _menhir_box_file) _menhir_cell1_ann_role -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DOT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_ann_role (_menhir_stack, _, _5) = _menhir_stack in
              let MenhirCell1_ann_name (_menhir_stack, _, _3) = _menhir_stack in
              let _7 = _v in
              let _v = _menhir_action_16 _3 _5 _7 in
              let _1 = _v in
              let _v = _menhir_action_60 _1 in
              _menhir_goto_top_entry _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_symbol_app (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | NEQ ->
          let _menhir_s = MenhirState114 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_s = MenhirState116 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | BAR | RPAREN ->
          let _v = _menhir_action_29 () in
          _menhir_goto_cnf_symbol_tail _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_cnf_symbol_tail : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_symbol_app -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_symbol_app (_menhir_stack, _menhir_s, sa) = _menhir_stack in
      let tail = _v in
      let _v = _menhir_action_21 sa tail in
      _menhir_goto_cnf_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_term_list -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState038 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | QUOTED _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_QUOTED as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_QUOTED (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_55 _1 _3 in
          _menhir_goto_symbol_app _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_term_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_58 _1 in
      _menhir_goto_term_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_044 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_11 _1 _3 in
      _menhir_goto_atomic_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_046 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_10 _1 _3 in
      _menhir_goto_atomic_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_symbol_app -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _2 = _v in
      let _v = _menhir_action_38 _2 in
      _menhir_goto_fof_symbol_tail _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_067 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_symbol_app -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _2 = _v in
      let _v = _menhir_action_37 _2 in
      _menhir_goto_fof_symbol_tail _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_105 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_24 _1 _3 in
      _menhir_goto_cnf_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_23 _1 _3 in
      _menhir_goto_cnf_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_symbol_app -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _2 = _v in
      let _v = _menhir_action_28 _2 in
      _menhir_goto_cnf_symbol_tail _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_symbol_app -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _2 = _v in
      let _v = _menhir_action_27 _2 in
      _menhir_goto_cnf_symbol_tail _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack _menhir_cell0_entries, _menhir_box_file) _menhir_cell1_ann_name as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ann_role (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState102 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | TILDE ->
              _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | QUOTED _v ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | LPAREN ->
              _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_TRUE ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | KW_FALSE ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState110 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TILDE ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | QUOTED _v ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | LPAREN ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_TRUE ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | KW_FALSE ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _v = _menhir_action_20 () in
          _menhir_goto_cnf_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BAR ->
          let _v = _menhir_action_25 () in
          _menhir_goto_cnf_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _v = _menhir_action_19 () in
          _menhir_goto_cnf_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BAR ->
          let _v = _menhir_action_26 () in
          _menhir_goto_cnf_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ann_name as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_08 _1 in
      _menhir_goto_ann_role _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ann_name as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_06 _1 in
      _menhir_goto_ann_role _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack _menhir_cell0_entries as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ann_name (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState100 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | QUOTED _v ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_30 () in
      _menhir_goto_entries _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
end

let file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_file v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
