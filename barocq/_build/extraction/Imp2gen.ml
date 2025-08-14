open Barray
open Datatypes
open Imp2
open List0
open Maps2
open Monads
open Syntax
open Types
open Typing
open Utils

(** val transl_statement : Imp1.Imp1Typed.statement -> statement **)

let rec transl_statement = function
| Imp1.Imp1Typed.StSet (x, c) ->
  (match c with
   | Typed.CpAtom (a, ty) -> StSetExpr (x, (EAtom (a, ty)))
   | Typed.CpArrayGet (a1, a2, ty) -> StSetExpr (x, (EArrayGet (a1, a2, ty)))
   | Typed.CpArraySet (a1, a2, a3, _) ->
     StSetEcomp (x, (EcArraySet (a1, a2, a3)))
   | Typed.CpRecordProj (a1, f, ty) ->
     StSetExpr (x, (ERecordProj (a1, f, ty)))
   | Typed.CpRecordUpdate (a1, f, a2, _) ->
     StSetEcomp (x, (EcRecordUpdate (a1, f, a2)))
   | Typed.CpDeepAccess (a, acs, ty) ->
     StSetExpr (x, (EDeepAccess (a, acs, ty)))
   | Typed.CpCall (a, args, _) -> StCall (x, a, args))
| Imp1.Imp1Typed.StIfThenElse (a, s1, s2) ->
  StIfThenElse (a, (transl_statement s1), (transl_statement s2))
| Imp1.Imp1Typed.StSequence (s1, s2) ->
  StSequence ((transl_statement s1), (transl_statement s2))
| Imp1.Imp1Typed.StReturn a -> StReturn a

(** val all_vars : Imp1.Imp1Typed.statement -> btyp SMapList.t **)

let rec all_vars = function
| Imp1.Imp1Typed.StSet (x, c) -> (x, (typof_comp c)) :: []
| Imp1.Imp1Typed.StIfThenElse (_, s1, s2) ->
  SMapList.merge (all_vars s1) (all_vars s2)
| Imp1.Imp1Typed.StSequence (s1, s2) ->
  SMapList.merge (all_vars s1) (all_vars s2)
| Imp1.Imp1Typed.StReturn _ -> []

(** val transl_function : Imp1.Imp1Typed.coq_function -> Imp2.coq_function **)

let transl_function f =
  { Imp2.fn_return = f.fn_return; Imp2.fn_params = f.fn_params; fn_vars =
    (all_vars f.fn_body); Imp2.fn_body = (transl_statement f.fn_body) }

(** val transl_array_lit :
    (Imp1.Imp1Typed.literal -> Imp2.literal SMapList.t ->
    (literal_base * Imp2.literal SMapList.t) MonCounter.coq_M) ->
    Imp1.Imp1Typed.literal array -> Imp2.literal SMapList.t -> (literal_base
    array * Imp2.literal SMapList.t) MonCounter.coq_M **)

let rec transl_array_lit transl_literal0 a defs =
  match a with
  | [] -> MonCounter.ret ([], defs)
  | lx :: a' ->
    MonCounter.bind2 (transl_literal0 lx defs) (fun lx' defs1 ->
      MonCounter.bind2 (transl_array_lit transl_literal0 a' defs1)
        (fun r defs2 -> MonCounter.ret ((lx' :: r), defs2)))

(** val transl_record_lit :
    (Imp1.Imp1Typed.literal -> Imp2.literal SMapList.t ->
    (literal_base * Imp2.literal SMapList.t) MonCounter.coq_M) ->
    Imp1.Imp1Typed.literal SMapList.t -> Imp2.literal SMapList.t ->
    (literal_base SMapList.t * Imp2.literal SMapList.t) MonCounter.coq_M **)

let rec transl_record_lit transl_literal0 rc defs =
  match rc with
  | [] -> MonCounter.ret ([], defs)
  | p :: rc' ->
    let (i, lx) = p in
    MonCounter.bind2 (transl_literal0 lx defs) (fun lx' defs1 ->
      MonCounter.bind2 (transl_record_lit transl_literal0 rc' defs1)
        (fun r defs2 -> MonCounter.ret (((i, lx') :: r), defs2)))

(** val fresh_var : ident MonCounter.coq_M **)

let fresh_var =
  fresh_var ('g'::[])

(** val transl_literal_rec :
    Imp1.Imp1Typed.literal -> Imp2.literal SMapList.t ->
    (literal_base * Imp2.literal SMapList.t) MonCounter.coq_M **)

let rec transl_literal_rec l defs =
  match l with
  | Typed.LTrue _ -> MonCounter.ret (LbTrue, defs)
  | Typed.LFalse _ -> MonCounter.ret (LbFalse, defs)
  | Typed.LInt32 (i, _) -> MonCounter.ret ((LbInt32 i), defs)
  | Typed.LInt64 (i, _) -> MonCounter.ret ((LbInt64 i), defs)
  | Typed.LArray (a, ty) ->
    MonCounter.bind2 (transl_array_lit transl_literal_rec a defs)
      (fun a' defs0 ->
      MonCounter.bind fresh_var (fun x ->
        MonCounter.ret ((LbVar x), ((x, (Imp2.LArray (a', ty))) :: defs0))))
  | Typed.LRecord (st, ty) ->
    MonCounter.bind2 (transl_record_lit transl_literal_rec st defs)
      (fun st' defs0 ->
      MonCounter.bind fresh_var (fun x ->
        MonCounter.ret ((LbVar x), ((x, (Imp2.LRecord (st', ty))) :: defs0))))

(** val transl_literal :
    Imp1.Imp1Typed.literal -> (Imp2.literal * Imp2.literal SMapList.t)
    MonCounter.coq_M **)

let transl_literal l =
  MonCounter.bind2
    (match l with
     | Typed.LArray (a, ty) ->
       MonCounter.bind2 (transl_array_lit transl_literal_rec a [])
         (fun a' defs -> MonCounter.ret ((Imp2.LArray (a', ty)), defs))
     | _ ->
       MonCounter.bind2 (transl_literal_rec l []) (fun stb defs ->
         MonCounter.ret ((LBase (stb, (typof_literal l))), defs)))
    (fun l' defs -> MonCounter.ret (l', (rev' defs)))

(** val typof_literal : Imp2.literal -> btyp **)

let typof_literal = function
| LBase (_, ty) -> ty
| Imp2.LArray (_, ty) -> ty
| Imp2.LRecord (_, ty) -> ty

(** val transl_globdefs_rec :
    Imp1.Imp1Typed.globdef list -> Imp2.globdef list MonCounter.coq_M **)

let rec transl_globdefs_rec = function
| [] -> MonCounter.ret []
| d :: defs' ->
  (match d with
   | DefConst (x, l, ty) ->
     MonCounter.bind2 (transl_literal l) (fun l' d1 ->
       let defs1 =
         map (fun pat ->
           let (x0, lx) = pat in DefConst (x0, lx, (typof_literal lx))) d1
       in
       MonCounter.bind (transl_globdefs_rec defs') (fun dr ->
         MonCounter.ret (app defs1 ((DefConst (x, l', ty)) :: dr))))
   | DefFun (x, f) ->
     MonCounter.bind (transl_globdefs_rec defs') (fun dr ->
       MonCounter.ret ((DefFun (x, (transl_function f))) :: dr))
   | DeclConst (x, ty) ->
     MonCounter.bind (transl_globdefs_rec defs') (fun dr ->
       MonCounter.ret ((DeclConst (x, ty)) :: dr))
   | DeclFun (f, tparams, tret) ->
     MonCounter.bind (transl_globdefs_rec defs') (fun dr ->
       MonCounter.ret ((DeclFun (f, tparams, tret)) :: dr)))

(** val transl_globdefs : Imp1.Imp1Typed.globdef list -> Imp2.globdef list **)

let transl_globdefs defs =
  fst (transl_globdefs_rec defs O)

(** val transl_program : Imp1.Imp1Typed.program -> Imp2.program **)

let transl_program prog =
  { prog_defs = (transl_globdefs prog.prog_defs); prog_types =
    prog.prog_types }
