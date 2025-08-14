open Barocq
open BarocqBNF
open BarocqTransf
open Datatypes
open Errors
open List0
open Monads
open Syntax
open Target
open Types
open Utils

(** val spread_atomlist : Barocq.expr -> BarocqBNF.atom list -> expr res **)

let spread_atomlist e la =
  match e with
  | ECast (_, ty) ->
    MonError.bind (nth_err la O) (fun a ->
      MonError.ret (EAtom (ACast (a, ty))))
  | EUnaryOp (op, _) ->
    MonError.bind (nth_err la O) (fun a ->
      MonError.ret (EAtom (AUnaryOp (op, a))))
  | EBinaryOp (op, _, _) ->
    MonError.bind (nth_err la O) (fun a1 ->
      MonError.bind (nth_err la (S O)) (fun a2 ->
        match op with
        | BopAndbool ->
          MonError.ret (EIfThenElse (a1, (EAtom a2), (EAtom AFalse)))
        | BopOrbool ->
          MonError.ret (EIfThenElse (a1, (EAtom ATrue), (EAtom a2)))
        | _ -> MonError.ret (EAtom (ABinaryOp (op, a1, a2)))))
  | Barocq.EArrayGet (_, _) ->
    MonError.bind (nth_err la O) (fun a1 ->
      MonError.bind (nth_err la (S O)) (fun a2 ->
        MonError.ret (EArrayGet (a1, a2))))
  | Barocq.EArraySet (_, _, _) ->
    MonError.bind (nth_err la O) (fun a1 ->
      MonError.bind (nth_err la (S O)) (fun a2 ->
        MonError.bind (nth_err la (S (S O))) (fun a3 ->
          MonError.ret (EArraySet (a1, a2, a3)))))
  | Barocq.ERecordProj (_, x) ->
    MonError.bind (nth_err la O) (fun a -> MonError.ret (ERecordProj (a, x)))
  | Barocq.ERecordUpdate (_, x, _) ->
    MonError.bind (nth_err la O) (fun a1 ->
      MonError.bind (nth_err la (S O)) (fun a2 ->
        MonError.ret (ERecordUpdate (a1, x, a2))))
  | Barocq.EApp (_, _) ->
    MonError.bind (nth_err la O) (fun a ->
      let args = tl la in MonError.ret (EApp (a, args)))
  | _ -> MonError.fail

(** val fresh_var : ident MonCounterErr.coq_M **)

let fresh_var =
  fresh_var_err ('b'::[])

(** val norm_expr_rec : Barocq.expr -> expr MonCounterErr.coq_M **)

let rec norm_expr_rec e =
  let norm_expr_aux =
    let rec norm_expr_aux ifc e0 = match e0 with
    | ETrue -> MonCounterErr.ret ([], ATrue)
    | EFalse -> MonCounterErr.ret ([], AFalse)
    | EInt32 (i, s) -> MonCounterErr.ret ([], (AInt32 (i, s)))
    | EInt64 (i, s) -> MonCounterErr.ret ([], (AInt64 (i, s)))
    | EVar x -> MonCounterErr.ret ([], (AVar x))
    | ECast (e1, ty) ->
      MonCounterErr.bind2 (norm_expr_aux ifc e1) (fun li1 a1 ->
        MonCounterErr.ret (li1, (ACast (a1, ty))))
    | EUnaryOp (op, e1) ->
      MonCounterErr.bind2 (norm_expr_aux ifc e1) (fun li a1 ->
        MonCounterErr.ret (li, (AUnaryOp (op, a1))))
    | EBinaryOp (op, e1, e2) ->
      if ifc
      then MonCounterErr.bind2 (norm_expr_aux ifc e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux ifc e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (ABinaryOp (op, a1, a2)))))
      else (match op with
            | BopAndbool ->
              MonCounterErr.bind fresh_var (fun x ->
                MonCounterErr.bind (norm_expr_rec e0) (fun be ->
                  MonCounterErr.ret (((x, be) :: []), (AVar x))))
            | BopOrbool ->
              MonCounterErr.bind fresh_var (fun x ->
                MonCounterErr.bind (norm_expr_rec e0) (fun be ->
                  MonCounterErr.ret (((x, be) :: []), (AVar x))))
            | _ ->
              MonCounterErr.bind2 (norm_expr_aux ifc e1) (fun li1 a1 ->
                MonCounterErr.bind2 (norm_expr_aux ifc e2) (fun li2 a2 ->
                  MonCounterErr.ret ((app li1 li2), (ABinaryOp (op, a1, a2))))))
    | _ ->
      MonCounterErr.bind fresh_var (fun x ->
        MonCounterErr.bind (norm_expr_rec e0) (fun be ->
          MonCounterErr.ret (((x, be) :: []), (AVar x))))
    in norm_expr_aux
  in
  let mk_norm =
    let rec mk_norm le e0 =
      match le with
      | [] -> e0
      | p :: le' -> let (x, be) = p in ELetIn (x, be, (mk_norm le' e0))
    in mk_norm
  in
  let norm_exprlist_rec =
    let rec norm_exprlist_rec e0 la = function
    | [] ->
      MonCounterErr.bind
        (MonCounterErr.lift_err (spread_atomlist e0 (rev' la))) (fun er ->
        MonCounterErr.ret ([], er))
    | e1 :: le' ->
      MonCounterErr.bind2 (norm_expr_aux false e1) (fun lx a1 ->
        MonCounterErr.bind2 (norm_exprlist_rec e0 (a1 :: la) le')
          (fun lr er -> MonCounterErr.ret ((app lx lr), er)))
    in norm_exprlist_rec
  in
  let norm_exprlist = fun e0 le ->
    MonCounterErr.bind2 (norm_exprlist_rec e0 [] le) (fun lx er ->
      MonCounterErr.ret (mk_norm lx er))
  in
  let norm_access_list_rec =
    let rec norm_access_list_rec a acs acs_norm =
      match acs with
      | [] -> MonCounterErr.ret ([], (EDeepAccess (a, (rev acs_norm))))
      | ac :: acs' ->
        (match ac with
         | Barocq.AcRecordField f ->
           norm_access_list_rec a acs' ((AcRecordField f) :: acs_norm)
         | Barocq.AcArrayIndex e0 ->
           MonCounterErr.bind2 (norm_expr_aux false e0) (fun lac ae ->
             MonCounterErr.bind2
               (norm_access_list_rec a acs' ((AcArrayIndex ae) :: acs_norm))
               (fun lr er -> MonCounterErr.ret ((app lac lr), er))))
    in norm_access_list_rec
  in
  let norm_deep_access = fun e0 acs ->
    MonCounterErr.bind2 (norm_expr_aux false e0) (fun le ba ->
      MonCounterErr.bind2 (norm_access_list_rec ba acs []) (fun lacs bacs ->
        MonCounterErr.ret (mk_norm (app le lacs) bacs)))
  in
  (match e with
   | ETrue -> MonCounterErr.ret (EAtom ATrue)
   | EFalse -> MonCounterErr.ret (EAtom AFalse)
   | EInt32 (i, s) -> MonCounterErr.ret (EAtom (AInt32 (i, s)))
   | EInt64 (i, s) -> MonCounterErr.ret (EAtom (AInt64 (i, s)))
   | EVar x -> MonCounterErr.ret (EAtom (AVar x))
   | ECast (e1, _) -> norm_exprlist e (e1 :: [])
   | EUnaryOp (_, e1) -> norm_exprlist e (e1 :: [])
   | EBinaryOp (_, e1, e2) -> norm_exprlist e (e1 :: (e2 :: []))
   | Barocq.EArrayGet (e1, e2) -> norm_exprlist e (e1 :: (e2 :: []))
   | Barocq.EArraySet (e1, e2, e3) ->
     norm_exprlist e (e1 :: (e2 :: (e3 :: [])))
   | Barocq.ERecordProj (e1, _) -> norm_exprlist e (e1 :: [])
   | Barocq.ERecordUpdate (e1, _, e2) -> norm_exprlist e (e1 :: (e2 :: []))
   | Barocq.EDeepAccess (e1, acs) -> norm_deep_access e1 acs
   | Barocq.EApp (e1, args) -> norm_exprlist e (e1 :: args)
   | Barocq.EIfThenElse (e1, e2, e3) ->
     MonCounterErr.bind2 (norm_expr_aux true e1) (fun le c ->
       MonCounterErr.bind (norm_expr_rec e2) (fun ne2 ->
         MonCounterErr.bind (norm_expr_rec e3) (fun ne3 ->
           MonCounterErr.ret (mk_norm le (EIfThenElse (c, ne2, ne3))))))
   | Barocq.ELetIn (x, e1, e2) ->
     MonCounterErr.bind (norm_expr_rec e1) (fun ne1 ->
       MonCounterErr.bind (norm_expr_rec e2) (fun ne2 ->
         MonCounterErr.ret (ELetIn (x, ne1, ne2)))))

(** val norm_expr : Barocq.expr -> expr res **)

let norm_expr e =
  MonError.bind (norm_expr_rec e O) (fun ne -> MonError.ret (fst ne))

(** val norm_function : Barocq.coq_function -> BarocqBNF.coq_function res **)

let norm_function f =
  MonError.bind (norm_expr f.fn_body) (fun body_norm ->
    MonError.ret { fn_return = f.fn_return; fn_params = f.fn_params;
      fn_body = body_norm })

(** val norm_program_rec :
    Barocq.program -> (BarocqBNF.globdef list * type_def list) res **)

let rec norm_program_rec = function
| [] -> MonError.ret ([], [])
| d :: prog' ->
  (match d with
   | DefType (a, fields) ->
     MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
       MonError.ret (ndefs, ((TdRecord { rd_name = a; rd_fields =
         fields }) :: types)))
   | Barocq.DefConst (x, l, ty) ->
     MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
       MonError.ret (((DefConst (x, l, ty)) :: ndefs), types))
   | Barocq.DefFun (x, f) ->
     MonError.bind (norm_function f) (fun f' ->
       MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
         MonError.ret (((DefFun (x, f')) :: ndefs), types)))
   | DeclType (t, tk) ->
     MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
       MonError.ret (ndefs, ((TdAbstract (t, tk)) :: types)))
   | Barocq.DeclConst (x, ty) ->
     MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
       MonError.ret (((DeclConst (x, ty)) :: ndefs), types))
   | Barocq.DeclFun (x, tparams, tret) ->
     MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
       MonError.ret (((DeclFun (x, tparams, tret)) :: ndefs), types)))

(** val norm_program : archi -> Barocq.program -> BarocqBNF.program res **)

let norm_program arch prog =
  MonError.bind (transf_program arch prog) (fun prog0 ->
    MonError.bind2 (norm_program_rec prog0) (fun defs types ->
      MonError.ret { prog_defs = defs; prog_types = types }))
