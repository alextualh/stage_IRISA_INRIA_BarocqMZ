open Datatypes
open Errors
open Imp1
open ImpABNF
open List0
open Monads
open Syntax
open Utils

(** val transl_statement : statement -> Imp1.statement **)

let rec transl_statement = function
| StSequence (s1, s2) ->
  let s1' = transl_statement s1 in
  let s2' = transl_statement s2 in Imp1.StSequence (s1', s2')
| StSet (x, c) -> Imp1.StSet (x, c)
| StIfThenElse (a, s1, s2) ->
  let s1' = transl_statement s1 in
  let s2' = transl_statement s2 in Imp1.StIfThenElse (a, s1', s2')

(** val fresh_var : ident MonCounter.coq_M **)

let fresh_var =
  fresh_var ('i'::[])

(** val transl_tailcomp_rec : tailcomp -> Imp1.statement MonCounter.coq_M **)

let rec transl_tailcomp_rec = function
| TcBegin (s, t1) ->
  let s' = transl_statement s in
  MonCounter.bind (transl_tailcomp_rec t1) (fun s1 ->
    MonCounter.ret (Imp1.StSequence (s', s1)))
| TcComp c ->
  (match c with
   | CpAtom a -> MonCounter.ret (StReturn a)
   | _ ->
     MonCounter.bind fresh_var (fun x ->
       let s = Imp1.StSequence ((Imp1.StSet (x, c)), (StReturn (AVar x))) in
       MonCounter.ret s))
| TcIfThenElse (a, t1, t2) ->
  MonCounter.bind (transl_tailcomp_rec t1) (fun t1' ->
    MonCounter.bind (transl_tailcomp_rec t2) (fun t2' ->
      MonCounter.ret (Imp1.StIfThenElse (a, t1', t2'))))

(** val transl_tailcomp : tailcomp -> Imp1.statement **)

let transl_tailcomp t =
  let (s, _) = transl_tailcomp_rec t O in s

(** val transl_function : ImpABNF.coq_function -> Imp1.coq_function **)

let transl_function f =
  { fn_return = f.fn_return; fn_params = f.fn_params; fn_body =
    (transl_tailcomp f.fn_body) }

(** val transl_globdef : ImpABNF.globdef -> Imp1.globdef **)

let transl_globdef = function
| DefConst (x, l, ty) -> DefConst (x, l, ty)
| DefFun (x, f) -> DefFun (x, (transl_function f))
| DeclConst (x, ty) -> DeclConst (x, ty)
| DeclFun (f, tparams, tret) -> DeclFun (f, tparams, tret)

(** val transl_program : ImpABNF.program -> Imp1.program **)

let transl_program prog =
  { prog_defs = (map transl_globdef prog.prog_defs); prog_types =
    prog.prog_types }

(** val gen_aliasing_program :
    bool -> Imp1Typed.program -> Aliasing_AST.program res **)

let gen_aliasing_program = Aliasing_impl.gen_aliasing_program

(** val check_program_aliasing :
    Aliasing_AST.program -> Imp1Typed.program res **)

let check_program_aliasing = Aliasing_check.check_program
