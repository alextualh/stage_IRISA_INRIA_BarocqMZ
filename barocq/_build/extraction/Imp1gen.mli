open Datatypes
open Errors
open Imp1
open ImpABNF
open List0
open Monads
open Syntax
open Utils

val transl_statement : statement -> Imp1.statement

val fresh_var : ident MonCounter.coq_M

val transl_tailcomp_rec : tailcomp -> Imp1.statement MonCounter.coq_M

val transl_tailcomp : tailcomp -> Imp1.statement

val transl_function : ImpABNF.coq_function -> Imp1.coq_function

val transl_globdef : ImpABNF.globdef -> Imp1.globdef

val transl_program : ImpABNF.program -> Imp1.program

val gen_aliasing_program :
  bool -> Imp1Typed.program -> Aliasing_AST.program res

val check_program_aliasing : Aliasing_AST.program -> Imp1Typed.program res
