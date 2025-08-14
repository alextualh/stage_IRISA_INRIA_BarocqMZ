open Barocq
open BarocqShallow
open Barray
open Datatypes
open Errors
open Ident
open List0
open Maps2
open Monads
open Syntax
open Target
open Types
open Utils

module Normalization :
 sig
  val spread_atomlist : expr -> BNF.atom list -> BNF.expr res

  val fresh_var : Syntax.ident MonCounterErr.coq_M

  val norm_expr_rec : expr -> BNF.expr MonCounterErr.coq_M

  val norm_expr : expr -> BNF.expr res

  val norm_function : Barocq.coq_function -> BNF.coq_function res

  val norm_program_rec :
    Barocq.program -> (BNF.globdef list * type_def list) res

  val norm_program : Barocq.program -> BNF.program res
 end

module Monadification :
 sig
  val arr_index_mtyp : archi -> Monadic.mtyp

  val typof_literal : Monadic.literal -> Monadic.mtyp

  val typof_atom : Monadic.atom -> Monadic.mtyp

  val typof_expr : Monadic.expr -> Monadic.mtyp

  val mtyp_eq_dec : Monadic.mtyp -> Monadic.mtyp -> bool

  type renv = Monadic.mtyp SMapList.t STree.t

  val renv_get : renv -> Syntax.ident -> Monadic.mtyp SMapList.t res

  val renv_update :
    renv -> Syntax.ident -> Monadic.mtyp SMapList.t -> renv res

  type gcontext = Monadic.mtyp STree.t

  type lcontext = Monadic.mtyp STree.t

  val gcontext_get : gcontext -> Syntax.ident -> Monadic.mtyp res

  val gcontext_update :
    gcontext -> Syntax.ident -> Monadic.mtyp -> gcontext res

  val lcontext_get : lcontext -> Syntax.ident -> Monadic.mtyp res

  val lcontext_update :
    lcontext -> Syntax.ident -> Monadic.mtyp -> lcontext res

  val fresh_var : Syntax.ident MonCounterErr.coq_M

  val make_lambda_args : nat -> Syntax.ident list MonCounterErr.coq_M

  val mtyp_list_eq_dec : Monadic.mtyp list -> Monadic.mtyp list -> bool

  val eta_expand_rec :
    Syntax.ident -> Monadic.mtyp -> Monadic.mtyp -> Syntax.ident list ->
    Monadic.atom MonCounterErr.coq_M

  val eta_expand :
    Syntax.ident -> Monadic.mtyp -> Monadic.mtyp -> Monadic.atom res

  val typof_var : gcontext -> lcontext -> Syntax.ident -> Monadic.mtyp res

  val typecheck_unary_op : unary_op -> Monadic.mtyp -> Monadic.mtyp res

  val typecheck_binary_op :
    binary_op -> Monadic.mtyp -> Monadic.mtyp -> Monadic.mtyp res

  val typecheck_array_get :
    archi -> Monadic.mtyp -> Monadic.mtyp -> Monadic.mtyp res

  val typecheck_atom_against :
    Monadic.atom -> Monadic.mtyp -> Monadic.atom res

  val typecheck_array_set :
    archi -> Monadic.mtyp -> Monadic.mtyp -> Monadic.atom ->
    (Monadic.atom * Monadic.mtyp) res

  val mtypof_field :
    Syntax.ident -> Monadic.mtyp SMapList.t -> Monadic.mtyp res

  val typecheck_record_proj :
    renv -> Monadic.mtyp -> Syntax.ident -> Monadic.mtyp res

  val typecheck_record_update :
    renv -> Monadic.mtyp -> Monadic.atom -> Syntax.ident ->
    (Monadic.atom * Monadic.mtyp) res

  val monadify_btyp : btyp -> Monadic.mtyp

  val record_id : Monadic.mtyp -> Syntax.ident res

  val typecheck_atom :
    renv -> gcontext -> lcontext -> BNF.atom -> Monadic.atom res

  val typecheck_call_rec :
    Monadic.mtyp list -> Monadic.atom list -> Monadic.mtyp -> (Monadic.atom
    list * Monadic.mtyp) res

  val typecheck_call :
    Monadic.mtyp -> Monadic.atom list -> (Monadic.atom list * Monadic.mtyp)
    res

  val wrap_mtyp : Monadic.mtyp -> Monadic.mtyp

  val unwrap_mtyp : Monadic.mtyp -> Monadic.mtyp

  val wrap_atom : Monadic.atom -> Monadic.expr res

  val monadify_expr_rec :
    archi -> renv -> gcontext -> lcontext -> BNF.expr -> bool -> Monadic.expr
    res

  val monadify_expr :
    archi -> renv -> gcontext -> lcontext -> BNF.expr -> Monadic.expr res

  val monadify_function :
    archi -> renv -> gcontext -> BNF.coq_function -> Monadic.coq_function res

  val make_absfun_tparams :
    (param_attr * btyp) list -> Monadic.mtyp list ->
    (param_attr * Monadic.mtyp) list res

  val typecheck_array_lit : Monadic.literal array -> Monadic.mtyp res

  val typecheck_struct_lit :
    Monadic.literal SMapList.t -> Monadic.mtyp SMapList.t -> bool

  val typecheck_literal : renv -> BNF.literal -> Monadic.literal res

  val monadify_globdefs_rec :
    archi -> renv -> gcontext -> BNF.globdef list -> Monadic.globdef list res

  val monadify_globdefs :
    archi -> renv -> BNF.globdef list -> Monadic.globdef list res

  val monadify_program : archi -> BNF.program -> Monadic.program res
 end

val monadify_norm_program : archi -> Barocq.program -> Monadic.program res
