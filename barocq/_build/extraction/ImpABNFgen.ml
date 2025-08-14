open Datatypes
open Errors
open ImpABNF
open ImpBNF
open Monads
open Nat
open Syntax

(** val tailcomp_depth : tailcomp -> nat **)

let rec tailcomp_depth = function
| TcBegin (s, t1) -> add (add (S O) (statement_depth s)) (tailcomp_depth t1)
| TcComp _ -> S (S (S O))
| TcIfThenElse (_, t1, t2) ->
  let m = max (S O) (tailcomp_depth t1) in
  let m0 = max m (tailcomp_depth t2) in add (S O) m0

(** val statement_depth : statement -> nat **)

and statement_depth = function
| StSetTailcomp (_, t) -> add (S O) (tailcomp_depth t)

(** val norm_statement : nat -> statement -> ImpABNF.statement res **)

let rec norm_statement fuel s =
  match fuel with
  | O -> MonError.fail
  | S fuel' ->
    let StSetTailcomp (x, t) = s in
    (match t with
     | TcBegin (s0, tc) ->
       MonError.bind (norm_statement fuel' s0) (fun s' ->
         MonError.bind (norm_statement fuel' (StSetTailcomp (x, tc)))
           (fun sc -> MonError.ret (StSequence (s', sc))))
     | TcComp c -> MonError.ret (StSet (x, c))
     | TcIfThenElse (a, t1, t2) ->
       MonError.bind (norm_statement fuel' (StSetTailcomp (x, t1)))
         (fun s1 ->
         MonError.bind (norm_statement fuel' (StSetTailcomp (x, t2)))
           (fun s2 -> MonError.ret (StIfThenElse (a, s1, s2)))))

(** val norm_tailcomp : tailcomp -> ImpABNF.tailcomp res **)

let rec norm_tailcomp = function
| TcBegin (s, t1) ->
  MonError.bind (norm_statement (add (statement_depth s) (S O)) s) (fun s' ->
    MonError.bind (norm_tailcomp t1) (fun t1' ->
      MonError.ret (ImpABNF.TcBegin (s', t1'))))
| TcComp c -> MonError.ret (ImpABNF.TcComp c)
| TcIfThenElse (a, t1, t2) ->
  MonError.bind (norm_tailcomp t1) (fun t1' ->
    MonError.bind (norm_tailcomp t2) (fun t2' ->
      MonError.ret (ImpABNF.TcIfThenElse (a, t1', t2'))))

(** val norm_function : ImpBNF.coq_function -> ImpABNF.coq_function res **)

let norm_function f =
  MonError.bind (norm_tailcomp f.fn_body) (fun body ->
    MonError.ret { fn_return = f.fn_return; fn_params = f.fn_params;
      fn_body = body })

(** val norm_globdef : ImpBNF.globdef -> ImpABNF.globdef res **)

let norm_globdef = function
| DefConst (x, l, ty) -> MonError.ret (DefConst (x, l, ty))
| DefFun (x, f) ->
  MonError.bind (norm_function f) (fun f' -> MonError.ret (DefFun (x, f')))
| DeclConst (x, ty) -> MonError.ret (DeclConst (x, ty))
| DeclFun (f, tparams, tret) -> MonError.ret (DeclFun (f, tparams, tret))

(** val norm_program : ImpBNF.program -> ImpABNF.program res **)

let norm_program prog =
  MonError.bind (mmap norm_globdef prog.prog_defs) (fun defs ->
    let prog' = { prog_defs = defs; prog_types = prog.prog_types } in
    MonError.ret prog')
