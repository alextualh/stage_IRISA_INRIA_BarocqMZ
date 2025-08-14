open AST
open BinNums
open Clight
open Ctypes

type expr = Clight.expr

type cexpr =
| CE_expr of expr
| CE_and of cexpr * cexpr
| CE_or of cexpr * cexpr
| CE_not of cexpr

type label = ident

type statement =
| Sskip
| Sassign of expr * expr
| Sset of ident * expr
| Scall of ident option * expr * expr list
| Sbuiltin of ident option * external_function * coq_type list * expr list
| Ssequence of statement * statement
| Sifthenelse of cexpr * statement * statement
| Sloop of statement * statement
| Sbreak
| Scontinue
| Sreturn of expr option
| Sswitch of expr * labeled_statements
| Slabel of label * statement
| Sgoto of label
and labeled_statements =
| LSnil
| LScons of coq_Z option * statement * labeled_statements

type coq_function = { fn_return : coq_type; fn_callconv : calling_convention;
                      fn_params : (ident * coq_type) list;
                      fn_vars : (ident * coq_type) list;
                      fn_temps : (ident * coq_type) list; fn_body : statement }

type fundef = coq_function Ctypes.fundef

type program = coq_function Ctypes.program
