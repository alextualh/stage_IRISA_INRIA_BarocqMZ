open BarocqBNF
open ImpBNF
open List0
open Syntax

(** val transl_expr : expr -> tailcomp **)

let rec transl_expr = function
| EAtom a -> TcComp (CpAtom a)
| EArrayGet (a1, a2) -> TcComp (CpArrayGet (a1, a2))
| EArraySet (a1, a2, a3) -> TcComp (CpArraySet (a1, a2, a3))
| ERecordProj (a, x) -> TcComp (CpRecordProj (a, x))
| ERecordUpdate (a1, x, a2) -> TcComp (CpRecordUpdate (a1, x, a2))
| EDeepAccess (a, acs) -> TcComp (CpDeepAccess (a, acs))
| EApp (a, args) -> TcComp (CpCall (a, args))
| EIfThenElse (a, e1, e2) ->
  TcIfThenElse (a, (transl_expr e1), (transl_expr e2))
| ELetIn (x, e1, e2) ->
  TcBegin ((StSetTailcomp (x, (transl_expr e1))), (transl_expr e2))

(** val transl_function : BarocqBNF.coq_function -> ImpBNF.coq_function **)

let transl_function f =
  { fn_return = f.fn_return; fn_params = f.fn_params; fn_body =
    (transl_expr f.fn_body) }

(** val transl_globdef : BarocqBNF.globdef -> ImpBNF.globdef **)

let transl_globdef = function
| DefConst (x, l, ty) -> DefConst (x, l, ty)
| DefFun (x, f) -> DefFun (x, (transl_function f))
| DeclConst (x, ty) -> DeclConst (x, ty)
| DeclFun (f, tparams, tret) -> DeclFun (f, tparams, tret)

(** val transl_program : BarocqBNF.program -> ImpBNF.program **)

let transl_program prog =
  { prog_defs = (map transl_globdef prog.prog_defs); prog_types =
    prog.prog_types }
