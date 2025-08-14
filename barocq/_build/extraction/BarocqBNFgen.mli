open Barocq
open BarocqBNF
open BarocqTransf
open Datatypes
open Errors
open List0
open Monads
open Syntax
open Target
open Types
open Utils

val spread_atomlist : Barocq.expr -> BarocqBNF.atom list -> expr res

val fresh_var : ident MonCounterErr.coq_M

val norm_expr_rec : Barocq.expr -> expr MonCounterErr.coq_M

val norm_expr : Barocq.expr -> expr res

val norm_function : Barocq.coq_function -> BarocqBNF.coq_function res

val norm_program_rec :
  Barocq.program -> (BarocqBNF.globdef list * type_def list) res

val norm_program : archi -> Barocq.program -> BarocqBNF.program res
