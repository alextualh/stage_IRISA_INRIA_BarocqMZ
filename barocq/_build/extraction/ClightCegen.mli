open AST
open Barray
open BinInt
open BinNums
open Clight
open ClightCe
open Cop
open Ctypes
open Ctypesdefs
open Datatypes
open Errors
open Ident
open Imp2
open Integers
open List0
open MSetPositive
open Maps
open Maps2
open Monads
open Syntax
open Types
open Utils

val transl_btyp : coq_type PMap.t -> btyp -> coq_type

val transl_btyp_to_xtype : btyp -> xtype

val transl_btyp_lit : coq_type PMap.t -> btyp -> coq_Z -> coq_type

val transl_literal_base : literal_base -> init_data

val transl_literal : Imp2.literal -> init_data list

val transl_unary_op : unary_op -> unary_operation res

val transl_binary_op : binary_op -> binary_operation res

val transl_atom :
  coq_type PMap.t -> PositiveSet.t -> Typed.atom -> ClightCe.expr res

val deref_pointer : coq_type -> coq_type

val transl_deep_access :
  coq_type PMap.t -> PositiveSet.t -> Imp2.atom -> Imp2.access list ->
  ClightCe.expr res

val transl_expr :
  coq_type PMap.t -> PositiveSet.t -> expr -> ClightCe.expr res

val transl_ecomp :
  coq_type PMap.t -> PositiveSet.t -> ecomp ->
  (ClightCe.expr * ClightCe.statement) res

val transl_cond_atom :
  coq_type PMap.t -> PositiveSet.t -> Imp2.atom -> cexpr res

val transl_statement :
  coq_type PMap.t -> PositiveSet.t -> statement -> ClightCe.statement res

val transl_function :
  coq_type PMap.t -> PositiveSet.t -> Imp2.coq_function ->
  ClightCe.coq_function res

val transl_abs_function :
  coq_type PMap.t -> Syntax.ident -> (param_attr * btyp) list -> btyp ->
  ClightCe.fundef

type cglobdef =
  AST.ident * (ClightCe.coq_function fundef, coq_type) AST.globdef

val literal_size : Imp2.literal -> coq_Z

val transl_globdefs_rec :
  coq_type PMap.t -> Imp2.globdef list -> PositiveSet.t -> cglobdef list res

val transl_globdefs :
  coq_type PMap.t -> Imp2.globdef list -> cglobdef list res

val transl_record_fields : coq_type PMap.t -> btyp SMapList.t -> members

val transl_struct_btyp :
  coq_type PMap.t -> Syntax.ident -> btyp SMapList.t -> composite_definition

val transl_prog_types :
  coq_type PMap.t -> type_def list -> composite_definition list

val public_idents : Imp2.globdef list -> Syntax.ident list

val _main : AST.ident

val mk_abs_types_impl : type_def list -> coq_type PMap.t

val transl_program : Imp2.program -> ClightCe.program res

val typ_idents_rec : PositiveSet.t -> coq_type -> PositiveSet.t

val typ_idents : coq_type -> PositiveSet.t

val function_idents : ClightCe.coq_function -> PositiveSet.t

val globdef_idents : cglobdef -> PositiveSet.t

val members_idents : members -> PositiveSet.t

val composite_idents : composite_definition -> PositiveSet.t

val program_idents : ClightCe.program -> AST.ident list
