open Datatypes
open Errors
open ImpABNF
open ImpBNF
open Monads
open Nat
open Syntax

val tailcomp_depth : tailcomp -> nat

val statement_depth : statement -> nat

val norm_statement : nat -> statement -> ImpABNF.statement res

val norm_tailcomp : tailcomp -> ImpABNF.tailcomp res

val norm_function : ImpBNF.coq_function -> ImpABNF.coq_function res

val norm_globdef : ImpBNF.globdef -> ImpABNF.globdef res

val norm_program : ImpBNF.program -> ImpABNF.program res
