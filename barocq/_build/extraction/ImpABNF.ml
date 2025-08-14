open Syntax

type literal = Syntax.literal

type atom = Syntax.atom

type statement =
| StSequence of statement * statement
| StSet of ident * comp
| StIfThenElse of atom * statement * statement

type tailcomp =
| TcBegin of statement * tailcomp
| TcComp of comp
| TcIfThenElse of atom * tailcomp * tailcomp

type coq_function = tailcomp Syntax.coq_function

type globdef = (literal, coq_function) Syntax.globdef

type program = globdef Syntax.program
