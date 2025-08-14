open Barray
open Datatypes
open Imp2
open List0
open Maps2
open Monads
open Syntax
open Types
open Typing
open Utils

val transl_statement : Imp1.Imp1Typed.statement -> statement

val all_vars : Imp1.Imp1Typed.statement -> btyp SMapList.t

val transl_function : Imp1.Imp1Typed.coq_function -> Imp2.coq_function

val transl_array_lit :
  (Imp1.Imp1Typed.literal -> Imp2.literal SMapList.t ->
  (literal_base * Imp2.literal SMapList.t) MonCounter.coq_M) ->
  Imp1.Imp1Typed.literal array -> Imp2.literal SMapList.t -> (literal_base
  array * Imp2.literal SMapList.t) MonCounter.coq_M

val transl_record_lit :
  (Imp1.Imp1Typed.literal -> Imp2.literal SMapList.t ->
  (literal_base * Imp2.literal SMapList.t) MonCounter.coq_M) ->
  Imp1.Imp1Typed.literal SMapList.t -> Imp2.literal SMapList.t ->
  (literal_base SMapList.t * Imp2.literal SMapList.t) MonCounter.coq_M

val fresh_var : ident MonCounter.coq_M

val transl_literal_rec :
  Imp1.Imp1Typed.literal -> Imp2.literal SMapList.t ->
  (literal_base * Imp2.literal SMapList.t) MonCounter.coq_M

val transl_literal :
  Imp1.Imp1Typed.literal -> (Imp2.literal * Imp2.literal SMapList.t)
  MonCounter.coq_M

val typof_literal : Imp2.literal -> btyp

val transl_globdefs_rec :
  Imp1.Imp1Typed.globdef list -> Imp2.globdef list MonCounter.coq_M

val transl_globdefs : Imp1.Imp1Typed.globdef list -> Imp2.globdef list

val transl_program : Imp1.Imp1Typed.program -> Imp2.program
