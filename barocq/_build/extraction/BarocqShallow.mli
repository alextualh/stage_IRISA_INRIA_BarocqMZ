open Barray
open Ctypes
open Integers
open List0
open Syntax
open Types

module BNF :
 sig
  type literal = Syntax.literal

  type atom =
  | ATrue
  | AFalse
  | AInt32 of Int.int * signedness
  | AInt64 of Int64.int * signedness
  | AVar of Syntax.ident
  | ACast of atom * btyp
  | AUnaryOp of unary_op * atom
  | ABinaryOp of binary_op * atom * atom
  | ARecordProj of atom * Syntax.ident
  | ARecordUpdate of atom * Syntax.ident * atom

  type expr =
  | EAtom of atom
  | EArrayGet of atom * atom
  | EArraySet of atom * atom * atom
  | EApp of atom * atom list
  | EIfThenElse of atom * expr * expr
  | ELetIn of Syntax.ident * expr * expr

  type coq_function = expr Syntax.coq_function

  type globdef = (literal, coq_function) Syntax.globdef

  type program = { prog_defs : globdef list; prog_types : type_def list }

  val prog_defs : program -> globdef list

  val prog_types : program -> type_def list
 end

module Monadic :
 sig
  type mtyp =
  | MBool
  | MInt32 of signedness
  | MInt64 of signedness
  | MArray of mtyp
  | MRecord of Syntax.ident
  | MFun of mtyp list * mtyp
  | MAbs of Syntax.ident
  | MRes of mtyp

  val mtyp_rect :
    'a1 -> (signedness -> 'a1) -> (signedness -> 'a1) -> (mtyp -> 'a1 -> 'a1)
    -> (Syntax.ident -> 'a1) -> (mtyp list -> mtyp -> 'a1 -> 'a1) ->
    (Syntax.ident -> 'a1) -> (mtyp -> 'a1 -> 'a1) -> mtyp -> 'a1

  type literal =
  | LTrue of mtyp
  | LFalse of mtyp
  | LInt32 of Int.int * mtyp
  | LInt64 of Int64.int * mtyp
  | LArray of literal array * mtyp
  | LRecord of (Syntax.ident * literal) list * mtyp

  type atom =
  | ATrue of mtyp
  | AFalse of mtyp
  | AInt32 of Int.int * mtyp
  | AInt64 of Int64.int * mtyp
  | AVar of Syntax.ident * mtyp
  | ACast of atom * mtyp
  | AUnaryOp of unary_op * atom * mtyp
  | ABinaryOp of binary_op * atom * atom * mtyp
  | ARecordProj of atom * Syntax.ident * mtyp
  | ARecordUpdate of atom * Syntax.ident * atom * mtyp
  | ALambda of Syntax.ident list * atom * mtyp
  | ALambdaRet of Syntax.ident list * atom * mtyp
  | AApp of Syntax.ident * Syntax.ident list * mtyp

  type expr =
  | EAtom of atom * mtyp
  | EArrayGet of atom * atom * mtyp
  | EArraySet of atom * atom * atom * mtyp
  | EApp of atom * atom list * mtyp
  | EIfThenElse of atom * expr * expr * mtyp
  | ELetIn of Syntax.ident * expr * expr * mtyp
  | ELetMon of Syntax.ident * expr * expr * mtyp
  | ERet of expr * mtyp

  type coq_function = { fn_return : mtyp;
                        fn_params : (Syntax.ident * mtyp) list; fn_body : 
                        expr }

  val fn_return : coq_function -> mtyp

  val fn_params : coq_function -> (Syntax.ident * mtyp) list

  type globdef =
  | DefConst of Syntax.ident * literal * mtyp
  | DefFun of Syntax.ident * coq_function
  | DeclConst of Syntax.ident * mtyp
  | DeclFun of Syntax.ident * (param_attr * mtyp) list * mtyp

  type record_def = { rd_name : Syntax.ident;
                      rd_fields : (Syntax.ident * mtyp) list }

  val rd_name : record_def -> Syntax.ident

  val rd_fields : record_def -> (Syntax.ident * mtyp) list

  type type_def =
  | TdRecord of record_def
  | TdAbstract of Syntax.ident * struct_or_union

  type program = { prog_defs : globdef list; prog_types : type_def list }

  val get_record_defs : type_def list -> record_def list
 end
