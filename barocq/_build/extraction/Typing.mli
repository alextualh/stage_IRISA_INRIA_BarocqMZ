open Barray
open Errors
open Maps2
open Monads
open Syntax
open Target
open Types

type renv = btyp SMapList.t STree.t

val renv_get : renv -> Syntax.ident -> btyp SMapList.t res

val renv_update : renv -> Syntax.ident -> btyp SMapList.t -> renv res

type tenv = typ SMapList.t STree.t

val tenv_get : tenv -> Syntax.ident -> typ SMapList.t res

val tenv_update : tenv -> Syntax.ident -> typ SMapList.t -> tenv res

val btyp_to_typ : tenv -> btyp -> typ res

val typof_literal : Typed.literal -> btyp

val typof_atom : Typed.atom -> btyp

val typof_comp : Typed.comp -> btyp

type gcontext = btyp STree.t

type lcontext = btyp STree.t

val gcontext_get : gcontext -> Syntax.ident -> btyp res

val gcontext_update : gcontext -> Syntax.ident -> btyp -> gcontext res

val lcontext_get : lcontext -> Syntax.ident -> btyp res

val lcontext_update : lcontext -> Syntax.ident -> btyp -> lcontext res

val typof_var : gcontext -> lcontext -> Syntax.ident -> btyp res

val typecheck_cast : btyp -> btyp -> btyp res

val typecheck_unary_op : unary_op -> btyp -> btyp res

val typecheck_binary_op : binary_op -> btyp -> btyp -> btyp res

val typecheck_array_get : archi -> btyp -> btyp -> btyp res

val typecheck_array_set : archi -> btyp -> btyp -> btyp -> btyp res

val typecheck_record_proj : renv -> btyp -> Syntax.ident -> btyp res

val typecheck_record_update : renv -> btyp -> btyp -> Syntax.ident -> btyp res

val typecheck_call_rec : btyp list -> btyp list -> btyp -> btyp res

val typecheck_call : btyp -> btyp list -> btyp res

val typecheck_array_lit : Typed.literal array -> btyp res

val typecheck_struct_lit : Typed.literal SMapList.t -> btyp SMapList.t -> bool

val typecheck_literal : renv -> literal -> Typed.literal res
