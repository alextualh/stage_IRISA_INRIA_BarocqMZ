open Integers
open Syntax
open Types

type literal_base =
| LbTrue
| LbFalse
| LbInt32 of Int.int
| LbInt64 of Int64.int
| LbVar of Syntax.ident

type literal =
| LBase of literal_base * btyp
| LArray of literal_base list * btyp
| LRecord of (Syntax.ident * literal_base) list * btyp

type atom = Typed.atom

type access = Typed.access

type expr =
| EAtom of atom * btyp
| EArrayGet of atom * atom * btyp
| ERecordProj of atom * Syntax.ident * btyp
| EDeepAccess of atom * access list * btyp

type ecomp =
| EcArraySet of atom * atom * atom
| EcRecordUpdate of atom * Syntax.ident * atom

type statement =
| StSkip
| StSetExpr of Syntax.ident * expr
| StSetEcomp of Syntax.ident * ecomp
| StCall of Syntax.ident * atom * atom list
| StIfThenElse of atom * statement * statement
| StSequence of statement * statement
| StReturn of atom

type coq_function = { fn_return : btyp;
                      fn_params : (Syntax.ident * btyp) list;
                      fn_vars : (Syntax.ident * btyp) list;
                      fn_body : statement }

type globdef = (literal, coq_function) Syntax.globdef

type program = globdef Syntax.program
