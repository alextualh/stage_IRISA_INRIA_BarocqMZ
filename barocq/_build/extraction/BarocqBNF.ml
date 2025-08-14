open Syntax

type literal = Syntax.literal

type atom = Syntax.atom

type expr =
| EAtom of atom
| EArrayGet of atom * atom
| EArraySet of atom * atom * atom
| ERecordProj of atom * ident
| ERecordUpdate of atom * ident * atom
| EDeepAccess of atom * access list
| EApp of atom * atom list
| EIfThenElse of atom * expr * expr
| ELetIn of ident * expr * expr

type coq_function = expr Syntax.coq_function

type globdef = (literal, coq_function) Syntax.globdef

type program = globdef Syntax.program
