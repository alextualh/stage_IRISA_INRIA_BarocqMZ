open Errors
open List0
open Maps2
open Monads
open Syntax
open Target
open Types
open Typing
open Utils

type literal = Syntax.literal

type atom = Syntax.atom

type statement =
| StSet of ident * comp
| StIfThenElse of atom * statement * statement
| StSequence of statement * statement
| StReturn of atom

type coq_function = statement Syntax.coq_function

type globdef = (literal, coq_function) Syntax.globdef

type program = globdef Syntax.program

module Typed :
 sig
  type literal = Typed.literal

  type atom = Typed.atom

  type comp = Typed.comp

  type statement =
  | StSet of ident * comp
  | StIfThenElse of atom * statement * statement
  | StSequence of statement * statement
  | StReturn of atom

  val statement_rect :
    (ident -> comp -> 'a1) -> (atom -> statement -> 'a1 -> statement -> 'a1
    -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> (atom -> 'a1)
    -> statement -> 'a1

  val statement_rec :
    (ident -> comp -> 'a1) -> (atom -> statement -> 'a1 -> statement -> 'a1
    -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> (atom -> 'a1)
    -> statement -> 'a1

  type coq_function = statement Syntax.coq_function

  type globdef = (literal, coq_function) Syntax.globdef

  type program = globdef Syntax.program
 end

module Aliasing_AST :
 sig
  type coq_ABSDOM = Aliasing_defs.AbsDom.t

  type literal = Syntax.Typed.literal

  type atom = Syntax.Typed.atom

  type comp = Syntax.Typed.comp

  type statement =
  | StSet of ident * comp * coq_ABSDOM * coq_ABSDOM
  | StIfThenElse of atom * statement * statement * coq_ABSDOM * coq_ABSDOM
  | StSequence of statement * statement
  | StReturn of atom * coq_ABSDOM * coq_ABSDOM

  type coq_function = statement Syntax.coq_function

  type globdef = (literal, coq_function) Syntax.globdef

  type program = globdef Syntax.program
 end

module Imp1Typed :
 sig
  type literal = Syntax.Typed.literal

  type atom = Syntax.Typed.atom

  type comp = Syntax.Typed.comp

  type statement = Typed.statement =
  | StSet of ident * comp
  | StIfThenElse of atom * statement * statement
  | StSequence of statement * statement
  | StReturn of atom

  val statement_rect :
    (ident -> comp -> 'a1) -> (atom -> statement -> 'a1 -> statement -> 'a1
    -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> (atom -> 'a1)
    -> statement -> 'a1

  val statement_rec :
    (ident -> comp -> 'a1) -> (atom -> statement -> 'a1 -> statement -> 'a1
    -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> (atom -> 'a1)
    -> statement -> 'a1

  type coq_function = statement Syntax.coq_function

  type globdef = (literal, coq_function) Syntax.globdef

  type program = globdef Syntax.program
 end

module Typing :
 sig
  val typof_var : gcontext -> lcontext -> ident -> btyp res

  val typecheck_atom :
    gcontext -> lcontext -> Syntax.atom -> Syntax.Typed.atom res

  val typecheck_access :
    archi -> renv -> gcontext -> lcontext -> btyp -> access list ->
    (btyp * Syntax.Typed.access list) res

  val typecheck_comp :
    archi -> renv -> gcontext -> lcontext -> comp -> Imp1Typed.comp res

  val merge_context : lcontext -> lcontext -> lcontext res

  val typecheck_statement :
    archi -> renv -> gcontext -> lcontext -> btyp -> statement ->
    (Imp1Typed.statement * lcontext) res

  val typecheck_function :
    archi -> renv -> gcontext -> coq_function -> Imp1Typed.coq_function res

  val typecheck_globdefs_rec :
    archi -> renv -> gcontext -> globdef list -> Imp1Typed.globdef list res

  val typecheck_globdefs :
    archi -> renv -> globdef list -> Imp1Typed.globdef list res

  val typecheck_program : archi -> program -> Imp1Typed.program res
 end

module Imp1Typing :
 sig
  val typof_var : gcontext -> lcontext -> ident -> btyp res

  val typecheck_atom :
    gcontext -> lcontext -> Syntax.atom -> Syntax.Typed.atom res

  val typecheck_access :
    archi -> renv -> gcontext -> lcontext -> btyp -> access list ->
    (btyp * Syntax.Typed.access list) res

  val typecheck_comp :
    archi -> renv -> gcontext -> lcontext -> comp -> Imp1Typed.comp res

  val merge_context : lcontext -> lcontext -> lcontext res

  val typecheck_statement :
    archi -> renv -> gcontext -> lcontext -> btyp -> statement ->
    (Imp1Typed.statement * lcontext) res

  val typecheck_function :
    archi -> renv -> gcontext -> coq_function -> Imp1Typed.coq_function res

  val typecheck_globdefs_rec :
    archi -> renv -> gcontext -> globdef list -> Imp1Typed.globdef list res

  val typecheck_globdefs :
    archi -> renv -> globdef list -> Imp1Typed.globdef list res

  val typecheck_program : archi -> program -> Imp1Typed.program res
 end
