open Barray
open Ctypes
open Ident
open Integers
open List0
open Types

type ident = t

type literal =
| LTrue
| LFalse
| LInt32 of Int.int * signedness
| LInt64 of Int64.int * signedness
| LArray of literal array
| LRecord of (ident * literal) list * ident

type unary_op =
| UopNotbool
| UopNotint
| UopNeg
| UopPlus

type binary_op =
| BopAndbool
| BopOrbool
| BopXorbool
| BopAdd
| BopSub
| BopMul
| BopDiv
| BopMod
| BopAndint
| BopOrint
| BopXorint
| BopShl
| BopShr
| BopEq
| BopNeq
| BopLt
| BopGt
| BopLe
| BopGe

type atom =
| ATrue
| AFalse
| AInt32 of Int.int * signedness
| AInt64 of Int64.int * signedness
| AVar of ident
| ACast of atom * btyp
| AUnaryOp of unary_op * atom
| ABinaryOp of binary_op * atom * atom

type access =
| AcRecordField of ident
| AcArrayIndex of atom

type comp =
| CpAtom of atom
| CpArrayGet of atom * atom
| CpArraySet of atom * atom * atom
| CpRecordProj of atom * ident
| CpRecordUpdate of atom * ident * atom
| CpDeepAccess of atom * access list
| CpCall of atom * atom list

module Typed :
 sig
  type literal =
  | LTrue of btyp
  | LFalse of btyp
  | LInt32 of Int.int * btyp
  | LInt64 of Int64.int * btyp
  | LArray of literal array * btyp
  | LRecord of (ident * literal) list * btyp

  type atom =
  | ATrue of btyp
  | AFalse of btyp
  | AInt32 of Int.int * btyp
  | AInt64 of Int64.int * btyp
  | AVar of ident * btyp
  | ACast of atom * btyp
  | AUnaryOp of unary_op * atom * btyp
  | ABinaryOp of binary_op * atom * atom * btyp

  type access =
  | AcRecordField of ident * btyp
  | AcArrayIndex of atom * btyp

  type comp =
  | CpAtom of atom * btyp
  | CpArrayGet of atom * atom * btyp
  | CpArraySet of atom * atom * atom * btyp
  | CpRecordProj of atom * ident * btyp
  | CpRecordUpdate of atom * ident * atom * btyp
  | CpDeepAccess of atom * access list * btyp
  | CpCall of atom * atom list * btyp
 end

type 'b coq_function = { fn_return : btyp; fn_params : (ident * btyp) list;
                         fn_body : 'b }

type param_attr =
| AttrReadonly
| AttrWrite
| AttrNone

type ('c, 'f) globdef =
| DefConst of ident * 'c * btyp
| DefFun of ident * 'f
| DeclConst of ident * btyp
| DeclFun of ident * (param_attr * btyp) list * btyp

type record_def = { rd_name : ident; rd_fields : (ident * btyp) list }

type type_def =
| TdRecord of record_def
| TdAbstract of ident * struct_or_union

type 'g program = { prog_defs : 'g list; prog_types : type_def list }

val get_record_defs : type_def list -> record_def list
