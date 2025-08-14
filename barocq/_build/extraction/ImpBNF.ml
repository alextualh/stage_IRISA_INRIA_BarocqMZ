open Syntax

type literal = Syntax.literal

type atom = Syntax.atom

type tailcomp =
| TcBegin of statement * tailcomp
| TcComp of comp
| TcIfThenElse of atom * tailcomp * tailcomp
and statement =
| StSetTailcomp of ident * tailcomp

type coq_function = tailcomp Syntax.coq_function

type globdef = (literal, coq_function) Syntax.globdef

type program = globdef Syntax.program
