open Barocq
open Datatypes
open Errors
open List0
open Monads
open Syntax
open Target
open Types
open Utils

val create_deep_access_expr : BarocqTyped.expr -> expr

val create_deep_access_function :
  BarocqTyped.coq_function -> Barocq.coq_function

val create_deep_access_globdef : BarocqTyped.globdef -> Barocq.globdef

val create_deep_access_program : BarocqTyped.program -> Barocq.program

val transf_program : archi -> Barocq.program -> Barocq.program res
