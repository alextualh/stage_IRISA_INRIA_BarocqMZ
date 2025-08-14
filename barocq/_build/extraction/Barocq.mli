open Barray
open Bool
open Brecord
open Ctypes
open Datatypes
open Errors
open Ident
open Integers
open Intop
open List0
open Maps
open Maps2
open Monads
open Syntax
open Target
open Types
open Typing
open Utils

type __ = Obj.t

type expr =
| ETrue
| EFalse
| EInt32 of Int.int * signedness
| EInt64 of Int64.int * signedness
| EVar of Syntax.ident
| ECast of expr * btyp
| EUnaryOp of unary_op * expr
| EBinaryOp of binary_op * expr * expr
| EArrayGet of expr * expr
| EArraySet of expr * expr * expr
| ERecordProj of expr * Syntax.ident
| ERecordUpdate of expr * Syntax.ident * expr
| EDeepAccess of expr * access list
| EApp of expr * expr list
| EIfThenElse of expr * expr * expr
| ELetIn of Syntax.ident * expr * expr
and access =
| AcRecordField of Syntax.ident
| AcArrayIndex of expr

type coq_function = expr Syntax.coq_function

type globdef =
| DefType of Syntax.ident * (Syntax.ident * btyp) list
| DefConst of Syntax.ident * literal * btyp
| DefFun of Syntax.ident * coq_function
| DeclType of Syntax.ident * struct_or_union
| DeclConst of Syntax.ident * btyp
| DeclFun of Syntax.ident * (param_attr * btyp) list * btyp

type command =
| CmdDef of globdef
| CmdExpr of expr

type program = globdef list

type iprogram = command list

val iprog_to_prog : iprogram -> program

module Typed :
 sig
  type expr =
  | ETrue of btyp
  | EFalse of btyp
  | EInt32 of Int.int * btyp
  | EInt64 of Int64.int * btyp
  | EVar of Syntax.ident * btyp
  | ECast of expr * btyp
  | EUnaryOp of unary_op * expr * btyp
  | EBinaryOp of binary_op * expr * expr * btyp
  | EArrayGet of expr * expr * btyp
  | EArraySet of expr * expr * expr * btyp
  | ERecordProj of expr * Syntax.ident * btyp
  | ERecordUpdate of expr * Syntax.ident * expr * btyp
  | EDeepAccess of expr * access list * btyp
  | EApp of expr * expr list * btyp
  | EIfThenElse of expr * expr * expr * btyp
  | ELetIn of Syntax.ident * expr * expr * btyp
  and access =
  | AcRecordField of Syntax.ident * btyp
  | AcArrayIndex of expr * btyp

  val expr_rect :
    (btyp -> 'a1) -> (btyp -> 'a1) -> (Int.int -> btyp -> 'a1) -> (Int64.int
    -> btyp -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 -> btyp
    -> 'a1) -> (unary_op -> expr -> 'a1 -> btyp -> 'a1) -> (binary_op -> expr
    -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 ->
    btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp ->
    'a1) -> (expr -> 'a1 -> Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 ->
    Syntax.ident -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> access
    list -> btyp -> 'a1) -> (expr -> 'a1 -> expr list -> btyp -> 'a1) ->
    (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) ->
    (Syntax.ident -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> expr -> 'a1

  val expr_rec :
    (btyp -> 'a1) -> (btyp -> 'a1) -> (Int.int -> btyp -> 'a1) -> (Int64.int
    -> btyp -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 -> btyp
    -> 'a1) -> (unary_op -> expr -> 'a1 -> btyp -> 'a1) -> (binary_op -> expr
    -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 ->
    btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp ->
    'a1) -> (expr -> 'a1 -> Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 ->
    Syntax.ident -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> access
    list -> btyp -> 'a1) -> (expr -> 'a1 -> expr list -> btyp -> 'a1) ->
    (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) ->
    (Syntax.ident -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> expr -> 'a1

  val access_rect :
    (Syntax.ident -> btyp -> 'a1) -> (expr -> btyp -> 'a1) -> access -> 'a1

  val access_rec :
    (Syntax.ident -> btyp -> 'a1) -> (expr -> btyp -> 'a1) -> access -> 'a1

  type coq_function = expr Syntax.coq_function

  type globdef =
  | DefType of Syntax.ident * btyp SMapList.t
  | DefConst of Syntax.ident * literal * btyp
  | DefFun of Syntax.ident * coq_function
  | DeclType of Syntax.ident * struct_or_union
  | DeclConst of Syntax.ident * btyp
  | DeclFun of Syntax.ident * (param_attr * btyp) list * btyp

  val globdef_rect :
    (Syntax.ident -> btyp SMapList.t -> 'a1) -> (Syntax.ident -> literal ->
    btyp -> 'a1) -> (Syntax.ident -> coq_function -> 'a1) -> (Syntax.ident ->
    struct_or_union -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (Syntax.ident
    -> (param_attr * btyp) list -> btyp -> 'a1) -> globdef -> 'a1

  val globdef_rec :
    (Syntax.ident -> btyp SMapList.t -> 'a1) -> (Syntax.ident -> literal ->
    btyp -> 'a1) -> (Syntax.ident -> coq_function -> 'a1) -> (Syntax.ident ->
    struct_or_union -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (Syntax.ident
    -> (param_attr * btyp) list -> btyp -> 'a1) -> globdef -> 'a1

  type program = globdef list
 end

module BarocqTyped :
 sig
  type expr = Typed.expr =
  | ETrue of btyp
  | EFalse of btyp
  | EInt32 of Int.int * btyp
  | EInt64 of Int64.int * btyp
  | EVar of Syntax.ident * btyp
  | ECast of expr * btyp
  | EUnaryOp of unary_op * expr * btyp
  | EBinaryOp of binary_op * expr * expr * btyp
  | EArrayGet of expr * expr * btyp
  | EArraySet of expr * expr * expr * btyp
  | ERecordProj of expr * Syntax.ident * btyp
  | ERecordUpdate of expr * Syntax.ident * expr * btyp
  | EDeepAccess of expr * access list * btyp
  | EApp of expr * expr list * btyp
  | EIfThenElse of expr * expr * expr * btyp
  | ELetIn of Syntax.ident * expr * expr * btyp
  and access = Typed.access =
  | AcRecordField of Syntax.ident * btyp
  | AcArrayIndex of expr * btyp

  val expr_rect :
    (btyp -> 'a1) -> (btyp -> 'a1) -> (Int.int -> btyp -> 'a1) -> (Int64.int
    -> btyp -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 -> btyp
    -> 'a1) -> (unary_op -> expr -> 'a1 -> btyp -> 'a1) -> (binary_op -> expr
    -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 ->
    btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp ->
    'a1) -> (expr -> 'a1 -> Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 ->
    Syntax.ident -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> access
    list -> btyp -> 'a1) -> (expr -> 'a1 -> expr list -> btyp -> 'a1) ->
    (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) ->
    (Syntax.ident -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> expr -> 'a1

  val expr_rec :
    (btyp -> 'a1) -> (btyp -> 'a1) -> (Int.int -> btyp -> 'a1) -> (Int64.int
    -> btyp -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 -> btyp
    -> 'a1) -> (unary_op -> expr -> 'a1 -> btyp -> 'a1) -> (binary_op -> expr
    -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 ->
    btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp ->
    'a1) -> (expr -> 'a1 -> Syntax.ident -> btyp -> 'a1) -> (expr -> 'a1 ->
    Syntax.ident -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> access
    list -> btyp -> 'a1) -> (expr -> 'a1 -> expr list -> btyp -> 'a1) ->
    (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) ->
    (Syntax.ident -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> expr -> 'a1

  val access_rect :
    (Syntax.ident -> btyp -> 'a1) -> (expr -> btyp -> 'a1) -> access -> 'a1

  val access_rec :
    (Syntax.ident -> btyp -> 'a1) -> (expr -> btyp -> 'a1) -> access -> 'a1

  type coq_function = expr Syntax.coq_function

  type globdef = Typed.globdef =
  | DefType of Syntax.ident * btyp SMapList.t
  | DefConst of Syntax.ident * literal * btyp
  | DefFun of Syntax.ident * coq_function
  | DeclType of Syntax.ident * struct_or_union
  | DeclConst of Syntax.ident * btyp
  | DeclFun of Syntax.ident * (param_attr * btyp) list * btyp

  val globdef_rect :
    (Syntax.ident -> btyp SMapList.t -> 'a1) -> (Syntax.ident -> literal ->
    btyp -> 'a1) -> (Syntax.ident -> coq_function -> 'a1) -> (Syntax.ident ->
    struct_or_union -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (Syntax.ident
    -> (param_attr * btyp) list -> btyp -> 'a1) -> globdef -> 'a1

  val globdef_rec :
    (Syntax.ident -> btyp SMapList.t -> 'a1) -> (Syntax.ident -> literal ->
    btyp -> 'a1) -> (Syntax.ident -> coq_function -> 'a1) -> (Syntax.ident ->
    struct_or_union -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (Syntax.ident
    -> (param_attr * btyp) list -> btyp -> 'a1) -> globdef -> 'a1

  type program = globdef list
 end

module Typing :
 sig
  val typof_expr : BarocqTyped.expr -> btyp

  val typecheck_deep_access :
    archi -> (renv -> gcontext -> lcontext -> expr -> BarocqTyped.expr res)
    -> renv -> gcontext -> lcontext -> btyp -> access list ->
    (btyp * Typed.access list) res

  val typecheck_expr :
    archi -> renv -> gcontext -> lcontext -> expr -> BarocqTyped.expr res

  val typecheck_function :
    archi -> archi -> renv -> gcontext -> coq_function ->
    BarocqTyped.coq_function res

  val typecheck_globdefs :
    archi -> renv -> gcontext -> globdef list -> BarocqTyped.globdef list res

  val typecheck_program : archi -> program -> BarocqTyped.program res
 end

type value =
| Val of typ * eval_typ

type access_value =
| AcvalRecordField of Syntax.ident
| AcvalArrayIndex of value

type genv = value STree.t

type lenv = value STree.t

val genv_get : __ PMap.t -> genv -> Syntax.ident -> value res

val genv_update : __ PMap.t -> genv -> Syntax.ident -> value -> genv res

val lenv_get : __ PMap.t -> lenv -> Syntax.ident -> value res

val lenv_update : __ PMap.t -> lenv -> Syntax.ident -> value -> lenv

val eval_var : __ PMap.t -> genv -> lenv -> Syntax.ident -> value res

val eval_cast : __ PMap.t -> value -> typ -> value res

val eval_unary_op : __ PMap.t -> unary_op -> value -> value res

val eval_binary_op : __ PMap.t -> binary_op -> value -> value -> value res

val eval_array_lit : __ PMap.t -> value array -> value res

val eval_record_lit_rec :
  __ PMap.t -> value SMapList.t -> typ SMapList.t -> eval_typ eval_recordtyp
  res

val eval_record_lit :
  __ PMap.t -> Syntax.ident -> value SMapList.t -> typ SMapList.t -> value res

val eval_array_get : archi -> __ PMap.t -> value -> value -> value res

val eval_array_set :
  archi -> __ PMap.t -> value -> value -> value -> value res

val proj_field : key -> 'a1 field -> 'a1

val eval_record_proj_aux :
  __ PMap.t -> typ SMapList.t -> eval_typ eval_recordtyp -> Syntax.ident ->
  value res

val eval_record_proj : __ PMap.t -> value -> Syntax.ident -> value res

val typof_field_dec : Syntax.ident -> typ SMapList.t -> typ res

val cast_typof_field :
  __ PMap.t -> Syntax.ident -> typ SMapList.t -> typ -> eval_typ ->
  type_of_field res

val eval_record_update_aux :
  __ PMap.t -> Syntax.ident -> typ SMapList.t -> eval_typ -> Syntax.ident ->
  value -> value res

val eval_record_update :
  __ PMap.t -> value -> Syntax.ident -> value -> value res

val eval_access_list :
  archi -> __ PMap.t -> value -> access_value list -> value res

val eval_ifthenelse :
  __ PMap.t -> value -> value res -> value res -> value res

val eval_app_rec :
  __ PMap.t -> typ list -> typ -> eval_typ eval_funtyp -> value list -> value
  res

val eval_app : __ PMap.t -> value -> value list -> value res

val eval_expr :
  archi -> __ PMap.t -> tenv -> genv -> lenv -> expr -> value res

val eval_literal : __ PMap.t -> tenv -> literal -> value res

val cast_typ_M : __ PMap.t -> typ -> value -> eval_typ MonError.coq_M

val build_funval_rec :
  archi -> __ PMap.t -> tenv -> genv -> lenv -> typ SMapList.t -> typ -> expr
  -> eval_typ eval_funtyp

val build_funval :
  archi -> __ PMap.t -> tenv -> genv -> typ SMapList.t -> typ -> expr ->
  eval_typ

val build_fun_value :
  archi -> __ PMap.t -> tenv -> genv -> btyp SMapList.t -> btyp -> expr ->
  value res

val fields_btyp_to_typ : tenv -> btyp SMapList.t -> typ SMapList.t res

val eval_def_type :
  tenv -> Syntax.ident -> (Syntax.ident * btyp) list -> tenv res

val eval_def_const :
  __ PMap.t -> tenv -> genv -> Syntax.ident -> literal -> btyp -> genv res

val eval_def_fun :
  archi -> __ PMap.t -> tenv -> genv -> Syntax.ident -> coq_function -> genv
  res

val interpret_rec :
  archi -> __ PMap.t -> tenv -> genv -> command list -> value list res

val interpret : archi -> __ PMap.t -> iprogram -> value list res

val eval_def_rec :
  archi -> __ PMap.t -> tenv -> genv -> program -> Syntax.ident -> value res

val eval_def :
  archi -> __ PMap.t -> genv -> program -> Syntax.ident -> value res
