open BarocqBNF
open ImpBNF
open List0
open Syntax

val transl_expr : expr -> tailcomp

val transl_function : BarocqBNF.coq_function -> ImpBNF.coq_function

val transl_globdef : BarocqBNF.globdef -> ImpBNF.globdef

val transl_program : BarocqBNF.program -> ImpBNF.program
