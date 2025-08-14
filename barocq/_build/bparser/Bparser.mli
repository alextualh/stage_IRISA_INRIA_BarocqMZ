
(* The type of tokens. *)

type token = 
  | WITH
  | TYP_UINT64
  | TYP_UINT32
  | TYP_INT64
  | TYP_INT32
  | TYP_BOOL
  | TYP_ARRAY
  | TYPE
  | TRUE
  | THEN
  | SEMISEMI
  | SEMICOLON
  | RPAREN
  | RBRACKETBAR
  | RBRACKET
  | RBRACE
  | OP_XORINT
  | OP_XORBOOL
  | OP_SHR
  | OP_SHL
  | OP_PLUS
  | OP_ORINT
  | OP_ORBOOL
  | OP_NOTINT
  | OP_NOTBOOL
  | OP_NEQ
  | OP_MUL
  | OP_MOD
  | OP_MINUS
  | OP_LT
  | OP_LE
  | OP_GT
  | OP_GE
  | OP_EQ
  | OP_DIV
  | OP_ANDINT
  | OP_ANDBOOL
  | OF
  | MODULE
  | LPAREN
  | LIT_STRING of (string)
  | LIT_INT64 of (int64 * Types.signedness)
  | LIT_INT32 of (int32 * Types.signedness)
  | LET
  | LBRACKETBAR
  | LBRACKET
  | LBRACE
  | IN
  | IMPORT
  | IF
  | IDENT of (string)
  | FALSE
  | EOF
  | ELSE
  | DOT
  | DEFN
  | DECL
  | COMPUTE
  | COMMA
  | COLON
  | BIND
  | AT_WRITE
  | AT_READONLY
  | AS
  | ARROW_INV
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val imodul: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SurfaceAST.imodul)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val imodul: Lexing.position -> (SurfaceAST.imodul) MenhirInterpreter.checkpoint
  
end
