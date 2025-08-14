open Barocq
open BarocqShallow
open Barray
open Datatypes
open Errors
open Ident
open List0
open Maps2
open Monads
open Syntax
open Target
open Types
open Utils

module Normalization =
 struct
  (** val spread_atomlist : expr -> BNF.atom list -> BNF.expr res **)

  let spread_atomlist e la =
    match e with
    | ECast (_, ty) ->
      MonError.bind (nth_err la O) (fun a ->
        MonError.ret (BNF.EAtom (BNF.ACast (a, ty))))
    | EUnaryOp (op, _) ->
      MonError.bind (nth_err la O) (fun a ->
        MonError.ret (BNF.EAtom (BNF.AUnaryOp (op, a))))
    | EBinaryOp (op, _, _) ->
      MonError.bind (nth_err la O) (fun a1 ->
        MonError.bind (nth_err la (S O)) (fun a2 ->
          MonError.ret (BNF.EAtom (BNF.ABinaryOp (op, a1, a2)))))
    | EArrayGet (_, _) ->
      MonError.bind (nth_err la O) (fun a1 ->
        MonError.bind (nth_err la (S O)) (fun a2 ->
          MonError.ret (BNF.EArrayGet (a1, a2))))
    | EArraySet (_, _, _) ->
      MonError.bind (nth_err la O) (fun a1 ->
        MonError.bind (nth_err la (S O)) (fun a2 ->
          MonError.bind (nth_err la (S (S O))) (fun a3 ->
            MonError.ret (BNF.EArraySet (a1, a2, a3)))))
    | ERecordProj (_, x) ->
      MonError.bind (nth_err la O) (fun a ->
        MonError.ret (BNF.EAtom (BNF.ARecordProj (a, x))))
    | ERecordUpdate (_, x, _) ->
      MonError.bind (nth_err la O) (fun a1 ->
        MonError.bind (nth_err la (S O)) (fun a2 ->
          MonError.ret (BNF.EAtom (BNF.ARecordUpdate (a1, x, a2)))))
    | EApp (_, _) ->
      MonError.bind (nth_err la O) (fun a ->
        let args = tl la in MonError.ret (BNF.EApp (a, args)))
    | _ -> MonError.fail

  (** val fresh_var : Syntax.ident MonCounterErr.coq_M **)

  let fresh_var =
    fresh_var_err ('b'::[])

  (** val norm_expr_rec : expr -> BNF.expr MonCounterErr.coq_M **)

  let rec norm_expr_rec e =
    let norm_expr_aux =
      let rec norm_expr_aux e0 = match e0 with
      | ETrue -> MonCounterErr.ret ([], BNF.ATrue)
      | EFalse -> MonCounterErr.ret ([], BNF.AFalse)
      | EInt32 (i, s) -> MonCounterErr.ret ([], (BNF.AInt32 (i, s)))
      | EInt64 (i, s) -> MonCounterErr.ret ([], (BNF.AInt64 (i, s)))
      | EVar x -> MonCounterErr.ret ([], (BNF.AVar x))
      | ECast (e1, ty) ->
        MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
          MonCounterErr.ret (li1, (BNF.ACast (a1, ty))))
      | EUnaryOp (op, e1) ->
        MonCounterErr.bind2 (norm_expr_aux e1) (fun li a1 ->
          MonCounterErr.ret (li, (BNF.AUnaryOp (op, a1))))
      | EBinaryOp (op, e1, e2) ->
        (match op with
         | BopAndbool ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2)))))
         | BopOrbool ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2)))))
         | BopXorbool ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2)))))
         | BopAdd ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2)))))
         | BopSub ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2)))))
         | BopMul ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2)))))
         | BopDiv ->
           MonCounterErr.bind fresh_var (fun x ->
             MonCounterErr.bind (norm_expr_rec e0) (fun be ->
               MonCounterErr.ret (((x, be) :: []), (BNF.AVar x))))
         | BopMod ->
           MonCounterErr.bind fresh_var (fun x ->
             MonCounterErr.bind (norm_expr_rec e0) (fun be ->
               MonCounterErr.ret (((x, be) :: []), (BNF.AVar x))))
         | _ ->
           MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
             MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
               MonCounterErr.ret ((app li1 li2), (BNF.ABinaryOp (op, a1, a2))))))
      | ERecordProj (e1, f) ->
        MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
          MonCounterErr.ret (li1, (BNF.ARecordProj (a1, f))))
      | ERecordUpdate (e1, f, e2) ->
        MonCounterErr.bind2 (norm_expr_aux e1) (fun li1 a1 ->
          MonCounterErr.bind2 (norm_expr_aux e2) (fun li2 a2 ->
            MonCounterErr.ret ((app li1 li2), (BNF.ARecordUpdate (a1, f, a2)))))
      | _ ->
        MonCounterErr.bind fresh_var (fun x ->
          MonCounterErr.bind (norm_expr_rec e0) (fun be ->
            MonCounterErr.ret (((x, be) :: []), (BNF.AVar x))))
      in norm_expr_aux
    in
    let mk_norm =
      let rec mk_norm le e0 =
        match le with
        | [] -> e0
        | p :: le' -> let (x, be) = p in BNF.ELetIn (x, be, (mk_norm le' e0))
      in mk_norm
    in
    let norm_exprlist_rec =
      let rec norm_exprlist_rec e0 la = function
      | [] ->
        MonCounterErr.bind
          (MonCounterErr.lift_err (spread_atomlist e0 (rev' la))) (fun er ->
          MonCounterErr.ret ([], er))
      | e1 :: le' ->
        MonCounterErr.bind2 (norm_expr_aux e1) (fun lx a1 ->
          MonCounterErr.bind2 (norm_exprlist_rec e0 (a1 :: la) le')
            (fun lr er -> MonCounterErr.ret ((app lx lr), er)))
      in norm_exprlist_rec
    in
    let norm_exprlist = fun e0 le ->
      MonCounterErr.bind2 (norm_exprlist_rec e0 [] le) (fun lx er ->
        MonCounterErr.ret (mk_norm lx er))
    in
    (match e with
     | ETrue -> MonCounterErr.ret (BNF.EAtom BNF.ATrue)
     | EFalse -> MonCounterErr.ret (BNF.EAtom BNF.AFalse)
     | EInt32 (i, s) -> MonCounterErr.ret (BNF.EAtom (BNF.AInt32 (i, s)))
     | EInt64 (i, s) -> MonCounterErr.ret (BNF.EAtom (BNF.AInt64 (i, s)))
     | EVar x -> MonCounterErr.ret (BNF.EAtom (BNF.AVar x))
     | ECast (e1, _) -> norm_exprlist e (e1 :: [])
     | EUnaryOp (_, e1) -> norm_exprlist e (e1 :: [])
     | EBinaryOp (_, e1, e2) -> norm_exprlist e (e1 :: (e2 :: []))
     | EArrayGet (e1, e2) -> norm_exprlist e (e1 :: (e2 :: []))
     | EArraySet (e1, e2, e3) -> norm_exprlist e (e1 :: (e2 :: (e3 :: [])))
     | ERecordProj (e1, _) -> norm_exprlist e (e1 :: [])
     | ERecordUpdate (e1, _, e2) -> norm_exprlist e (e1 :: (e2 :: []))
     | EDeepAccess (_, _) -> MonCounterErr.fail
     | EApp (e1, args) -> norm_exprlist e (e1 :: args)
     | EIfThenElse (e1, e2, e3) ->
       MonCounterErr.bind2 (norm_expr_aux e1) (fun le c ->
         MonCounterErr.bind (norm_expr_rec e2) (fun ne2 ->
           MonCounterErr.bind (norm_expr_rec e3) (fun ne3 ->
             MonCounterErr.ret (mk_norm le (BNF.EIfThenElse (c, ne2, ne3))))))
     | ELetIn (x, e1, e2) ->
       MonCounterErr.bind (norm_expr_rec e1) (fun ne1 ->
         MonCounterErr.bind (norm_expr_rec e2) (fun ne2 ->
           MonCounterErr.ret (BNF.ELetIn (x, ne1, ne2)))))

  (** val norm_expr : expr -> BNF.expr res **)

  let norm_expr e =
    MonError.bind (norm_expr_rec e O) (fun ne -> MonError.ret (fst ne))

  (** val norm_function : Barocq.coq_function -> BNF.coq_function res **)

  let norm_function f =
    MonError.bind (norm_expr f.fn_body) (fun body_norm ->
      MonError.ret { fn_return = f.fn_return; fn_params = f.fn_params;
        fn_body = body_norm })

  (** val norm_program_rec :
      Barocq.program -> (BNF.globdef list * type_def list) res **)

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
     | DeclType (t0, tk) ->
       MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
         MonError.ret (ndefs, ((TdAbstract (t0, tk)) :: types)))
     | Barocq.DeclConst (x, ty) ->
       MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
         MonError.ret (((DeclConst (x, ty)) :: ndefs), types))
     | Barocq.DeclFun (f, tparams, tret) ->
       MonError.bind2 (norm_program_rec prog') (fun ndefs types ->
         MonError.ret (((DeclFun (f, tparams, tret)) :: ndefs), types)))

  (** val norm_program : Barocq.program -> BNF.program res **)

  let norm_program prog =
    MonError.bind2 (norm_program_rec prog) (fun defs types ->
      MonError.ret { BNF.prog_defs = defs; BNF.prog_types = types })
 end

module Monadification =
 struct
  (** val arr_index_mtyp : archi -> Monadic.mtyp **)

  let arr_index_mtyp = function
  | Ptr32 -> Monadic.MInt32 Unsigned
  | Ptr64 -> Monadic.MInt64 Unsigned

  (** val typof_literal : Monadic.literal -> Monadic.mtyp **)

  let typof_literal = function
  | Monadic.LTrue ty -> ty
  | Monadic.LFalse ty -> ty
  | Monadic.LInt32 (_, ty) -> ty
  | Monadic.LInt64 (_, ty) -> ty
  | Monadic.LArray (_, ty) -> ty
  | Monadic.LRecord (_, ty) -> ty

  (** val typof_atom : Monadic.atom -> Monadic.mtyp **)

  let typof_atom = function
  | Monadic.ATrue ty -> ty
  | Monadic.AFalse ty -> ty
  | Monadic.AInt32 (_, ty) -> ty
  | Monadic.AInt64 (_, ty) -> ty
  | Monadic.AVar (_, ty) -> ty
  | Monadic.ACast (_, ty) -> ty
  | Monadic.AUnaryOp (_, _, ty) -> ty
  | Monadic.ABinaryOp (_, _, _, ty) -> ty
  | Monadic.ARecordProj (_, _, ty) -> ty
  | Monadic.ARecordUpdate (_, _, _, ty) -> ty
  | Monadic.ALambda (_, _, ty) -> ty
  | Monadic.ALambdaRet (_, _, ty) -> ty
  | Monadic.AApp (_, _, ty) -> ty

  (** val typof_expr : Monadic.expr -> Monadic.mtyp **)

  let typof_expr = function
  | Monadic.EAtom (_, ty) -> ty
  | Monadic.EArrayGet (_, _, ty) -> ty
  | Monadic.EArraySet (_, _, _, ty) -> ty
  | Monadic.EApp (_, _, ty) -> ty
  | Monadic.EIfThenElse (_, _, _, ty) -> ty
  | Monadic.ELetIn (_, _, _, ty) -> ty
  | Monadic.ELetMon (_, _, _, ty) -> ty
  | Monadic.ERet (_, ty) -> ty

  (** val mtyp_eq_dec : Monadic.mtyp -> Monadic.mtyp -> bool **)

  let rec mtyp_eq_dec t1 t2 =
    match t1 with
    | Monadic.MBool -> (match t2 with
                        | Monadic.MBool -> true
                        | _ -> false)
    | Monadic.MInt32 s ->
      (match t2 with
       | Monadic.MInt32 s0 -> signedness_eq_dec s s0
       | _ -> false)
    | Monadic.MInt64 s ->
      (match t2 with
       | Monadic.MInt64 s0 -> signedness_eq_dec s s0
       | _ -> false)
    | Monadic.MArray m ->
      (match t2 with
       | Monadic.MArray m0 -> mtyp_eq_dec m m0
       | _ -> false)
    | Monadic.MRecord i ->
      (match t2 with
       | Monadic.MRecord i0 -> eq_dec i i0
       | _ -> false)
    | Monadic.MFun (l, m) ->
      (match t2 with
       | Monadic.MFun (l0, m0) ->
         if list_eq_dec mtyp_eq_dec l l0 then mtyp_eq_dec m m0 else false
       | _ -> false)
    | Monadic.MAbs i ->
      (match t2 with
       | Monadic.MAbs i0 -> eq_dec i i0
       | _ -> false)
    | Monadic.MRes m ->
      (match t2 with
       | Monadic.MRes m0 -> mtyp_eq_dec m m0
       | _ -> false)

  type renv = Monadic.mtyp SMapList.t STree.t

  (** val renv_get : renv -> Syntax.ident -> Monadic.mtyp SMapList.t res **)

  let renv_get re x =
    MonError.err_of_opt (STree.get x re)

  (** val renv_update :
      renv -> Syntax.ident -> Monadic.mtyp SMapList.t -> renv res **)

  let renv_update re x fields =
    match renv_get re x with
    | OK _ -> MonError.fail
    | Error _ -> MonError.ret (STree.set x fields re)

  type gcontext = Monadic.mtyp STree.t

  type lcontext = Monadic.mtyp STree.t

  (** val gcontext_get : gcontext -> Syntax.ident -> Monadic.mtyp res **)

  let gcontext_get gx x =
    match STree.get x gx with
    | Some t0 -> MonError.ret t0
    | None -> MonError.fail

  (** val gcontext_update :
      gcontext -> Syntax.ident -> Monadic.mtyp -> gcontext res **)

  let gcontext_update gx x ty =
    match gcontext_get gx x with
    | OK _ -> MonError.fail
    | Error _ -> MonError.ret (STree.set x ty gx)

  (** val lcontext_get : lcontext -> Syntax.ident -> Monadic.mtyp res **)

  let lcontext_get lx x =
    match STree.get x lx with
    | Some t0 -> MonError.ret t0
    | None -> MonError.fail

  (** val lcontext_update :
      lcontext -> Syntax.ident -> Monadic.mtyp -> lcontext res **)

  let lcontext_update lx x ty =
    match lcontext_get lx x with
    | OK t0 ->
      if mtyp_eq_dec ty t0
      then MonError.ret (STree.set x ty lx)
      else MonError.fail
    | Error _ -> MonError.ret (STree.set x ty lx)

  (** val fresh_var : Syntax.ident MonCounterErr.coq_M **)

  let fresh_var =
    fresh_var_err ('x'::[])

  (** val make_lambda_args : nat -> Syntax.ident list MonCounterErr.coq_M **)

  let rec make_lambda_args = function
  | O -> MonCounterErr.ret []
  | S n' ->
    MonCounterErr.bind fresh_var (fun x ->
      MonCounterErr.bind (make_lambda_args n') (fun r ->
        MonCounterErr.ret (x :: r)))

  (** val mtyp_list_eq_dec :
      Monadic.mtyp list -> Monadic.mtyp list -> bool **)

  let mtyp_list_eq_dec =
    list_eq_dec mtyp_eq_dec

  (** val eta_expand_rec :
      Syntax.ident -> Monadic.mtyp -> Monadic.mtyp -> Syntax.ident list ->
      Monadic.atom MonCounterErr.coq_M **)

  let rec eta_expand_rec f ty1 ty2 l =
    match ty1 with
    | Monadic.MBool ->
      (match ty2 with
       | Monadic.MBool -> MonCounterErr.ret (Monadic.AApp (f, l, ty2))
       | _ -> MonCounterErr.fail)
    | Monadic.MInt32 _ ->
      (match ty2 with
       | Monadic.MInt32 _ -> MonCounterErr.ret (Monadic.AApp (f, l, ty2))
       | _ -> MonCounterErr.fail)
    | Monadic.MInt64 _ ->
      (match ty2 with
       | Monadic.MInt64 _ -> MonCounterErr.ret (Monadic.AApp (f, l, ty2))
       | _ -> MonCounterErr.fail)
    | Monadic.MArray ta1 ->
      (match ty2 with
       | Monadic.MArray ta2 ->
         if mtyp_eq_dec ta1 ta2
         then MonCounterErr.ret (Monadic.AApp (f, l, ty2))
         else MonCounterErr.fail
       | _ -> MonCounterErr.fail)
    | Monadic.MRecord r1 ->
      (match ty2 with
       | Monadic.MRecord r2 ->
         if eq_dec r1 r2
         then MonCounterErr.ret (Monadic.AApp (f, l, ty2))
         else MonCounterErr.fail
       | _ -> MonCounterErr.fail)
    | Monadic.MFun (t1, tr1) ->
      (match ty2 with
       | Monadic.MFun (t2, tr2) ->
         if mtyp_list_eq_dec t1 t2
         then if mtyp_eq_dec tr1 tr2
              then MonCounterErr.ret (Monadic.AApp (f, l, ty2))
              else (match tr2 with
                    | Monadic.MRes tr2' ->
                      MonCounterErr.bind (make_lambda_args (length t1))
                        (fun args ->
                        MonCounterErr.bind
                          (eta_expand_rec f tr1 tr2' (app l args)) (fun a ->
                          MonCounterErr.ret (Monadic.ALambdaRet (args, a,
                            ty2))))
                    | _ ->
                      MonCounterErr.bind (make_lambda_args (length t1))
                        (fun args ->
                        MonCounterErr.bind
                          (eta_expand_rec f tr1 tr2 (app l args)) (fun a ->
                          MonCounterErr.ret (Monadic.ALambda (args, a, ty2)))))
         else MonCounterErr.fail
       | _ -> MonCounterErr.fail)
    | Monadic.MAbs t1 ->
      (match ty2 with
       | Monadic.MAbs t2 ->
         if eq_dec t1 t2
         then MonCounterErr.ret (Monadic.AApp (f, l, ty1))
         else MonCounterErr.fail
       | _ -> MonCounterErr.fail)
    | Monadic.MRes _ -> MonCounterErr.fail

  (** val eta_expand :
      Syntax.ident -> Monadic.mtyp -> Monadic.mtyp -> Monadic.atom res **)

  let eta_expand f ty1 ty2 =
    match eta_expand_rec f ty1 ty2 [] O with
    | OK p -> let (a, _) = p in OK a
    | Error e -> Error e

  (** val typof_var :
      gcontext -> lcontext -> Syntax.ident -> Monadic.mtyp res **)

  let typof_var gx lx x =
    match lcontext_get lx x with
    | OK ty -> MonError.ret ty
    | Error _ -> gcontext_get gx x

  (** val typecheck_unary_op :
      unary_op -> Monadic.mtyp -> Monadic.mtyp res **)

  let typecheck_unary_op op ty =
    match op with
    | UopNotbool ->
      (match ty with
       | Monadic.MBool -> MonError.ret ty
       | _ -> MonError.fail)
    | UopPlus -> MonError.fail
    | _ ->
      (match ty with
       | Monadic.MInt32 _ -> MonError.ret ty
       | Monadic.MInt64 _ -> MonError.ret ty
       | _ -> MonError.fail)

  (** val typecheck_binary_op :
      binary_op -> Monadic.mtyp -> Monadic.mtyp -> Monadic.mtyp res **)

  let typecheck_binary_op op ty1 ty2 =
    match op with
    | BopAndbool ->
      (match ty1 with
       | Monadic.MBool ->
         (match ty2 with
          | Monadic.MBool -> MonError.ret ty1
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopOrbool ->
      (match ty1 with
       | Monadic.MBool ->
         (match ty2 with
          | Monadic.MBool -> MonError.ret ty1
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopXorbool ->
      (match ty1 with
       | Monadic.MBool ->
         (match ty2 with
          | Monadic.MBool -> MonError.ret ty1
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopEq ->
      (match ty1 with
       | Monadic.MBool ->
         (match ty2 with
          | Monadic.MBool -> MonError.ret ty1
          | _ -> MonError.fail)
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopNeq ->
      (match ty1 with
       | Monadic.MBool ->
         (match ty2 with
          | Monadic.MBool -> MonError.ret ty1
          | _ -> MonError.fail)
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopLt ->
      (match ty1 with
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopGt ->
      (match ty1 with
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopLe ->
      (match ty1 with
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | BopGe ->
      (match ty1 with
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then MonError.ret Monadic.MBool
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)
    | _ ->
      (match ty1 with
       | Monadic.MInt32 s1 ->
         (match ty2 with
          | Monadic.MInt32 s2 ->
            if signedness_eq_dec s1 s2
            then (match op with
                  | BopDiv -> MonError.ret (Monadic.MRes ty1)
                  | BopMod -> MonError.ret (Monadic.MRes ty1)
                  | _ -> MonError.ret ty1)
            else MonError.fail
          | _ -> MonError.fail)
       | Monadic.MInt64 s1 ->
         (match ty2 with
          | Monadic.MInt64 s2 ->
            if signedness_eq_dec s1 s2
            then (match op with
                  | BopDiv -> MonError.ret (Monadic.MRes ty1)
                  | BopMod -> MonError.ret (Monadic.MRes ty1)
                  | _ -> MonError.ret ty1)
            else MonError.fail
          | _ -> MonError.fail)
       | _ -> MonError.fail)

  (** val typecheck_array_get :
      archi -> Monadic.mtyp -> Monadic.mtyp -> Monadic.mtyp res **)

  let typecheck_array_get arch ty1 ty2 =
    match ty1 with
    | Monadic.MArray ta ->
      if mtyp_eq_dec ty2 (arr_index_mtyp arch)
      then MonError.ret (Monadic.MRes ta)
      else MonError.fail
    | _ -> MonError.fail

  (** val typecheck_atom_against :
      Monadic.atom -> Monadic.mtyp -> Monadic.atom res **)

  let typecheck_atom_against a ty =
    match a with
    | Monadic.AVar (x, tx) ->
      (match tx with
       | Monadic.MFun (_, _) -> eta_expand x tx ty
       | _ ->
         if mtyp_eq_dec (typof_atom a) ty
         then MonError.ret a
         else MonError.fail)
    | _ ->
      if mtyp_eq_dec (typof_atom a) ty then MonError.ret a else MonError.fail

  (** val typecheck_array_set :
      archi -> Monadic.mtyp -> Monadic.mtyp -> Monadic.atom ->
      (Monadic.atom * Monadic.mtyp) res **)

  let typecheck_array_set arch ty1 ty2 a3 =
    let tr = Monadic.MRes ty1 in
    (match ty1 with
     | Monadic.MArray ta ->
       if mtyp_eq_dec ty2 (arr_index_mtyp arch)
       then MonError.bind (typecheck_atom_against a3 ta) (fun a3' ->
              MonError.ret (a3', tr))
       else MonError.fail
     | _ -> MonError.fail)

  (** val mtypof_field :
      Syntax.ident -> Monadic.mtyp SMapList.t -> Monadic.mtyp res **)

  let mtypof_field =
    SMapList.find_err

  (** val typecheck_record_proj :
      renv -> Monadic.mtyp -> Syntax.ident -> Monadic.mtyp res **)

  let typecheck_record_proj re ty x =
    match ty with
    | Monadic.MRecord t0 ->
      MonError.bind (renv_get re t0) (fun fields -> mtypof_field x fields)
    | _ -> MonError.fail

  (** val typecheck_record_update :
      renv -> Monadic.mtyp -> Monadic.atom -> Syntax.ident ->
      (Monadic.atom * Monadic.mtyp) res **)

  let typecheck_record_update re ty1 a2 x =
    match ty1 with
    | Monadic.MRecord t0 ->
      MonError.bind (renv_get re t0) (fun fields ->
        MonError.bind (mtypof_field x fields) (fun tx ->
          MonError.bind (typecheck_atom_against a2 tx) (fun a2' ->
            MonError.ret (a2', ty1))))
    | _ -> MonError.fail

  (** val monadify_btyp : btyp -> Monadic.mtyp **)

  let rec monadify_btyp = function
  | BBool -> Monadic.MBool
  | BInt32 s -> Monadic.MInt32 s
  | BInt64 s -> Monadic.MInt64 s
  | BArray ta -> Monadic.MArray (monadify_btyp ta)
  | BRecord s -> Monadic.MRecord s
  | BFun (tparams, tret) ->
    Monadic.MFun ((map monadify_btyp tparams), (Monadic.MRes
      (monadify_btyp tret)))
  | BAbs t0 -> Monadic.MAbs t0

  (** val record_id : Monadic.mtyp -> Syntax.ident res **)

  let record_id = function
  | Monadic.MRecord rid -> MonError.ret rid
  | _ -> MonError.fail

  (** val typecheck_atom :
      renv -> gcontext -> lcontext -> BNF.atom -> Monadic.atom res **)

  let rec typecheck_atom re gx lx = function
  | BNF.ATrue -> MonError.ret (Monadic.ATrue Monadic.MBool)
  | BNF.AFalse -> MonError.ret (Monadic.AFalse Monadic.MBool)
  | BNF.AInt32 (i, s) -> MonError.ret (Monadic.AInt32 (i, (Monadic.MInt32 s)))
  | BNF.AInt64 (i, s) -> MonError.ret (Monadic.AInt64 (i, (Monadic.MInt64 s)))
  | BNF.AVar x ->
    MonError.bind (typof_var gx lx x) (fun t0 ->
      MonError.ret (Monadic.AVar (x, t0)))
  | BNF.ACast (a1, ty) ->
    MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
      let t0 = monadify_btyp ty in
      if mtyp_eq_dec (typof_atom a1') t0
      then MonError.ret a1'
      else MonError.ret (Monadic.ACast (a1', t0)))
  | BNF.AUnaryOp (op, a1) ->
    MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
      let ty1 = typof_atom a1' in
      MonError.bind (typecheck_unary_op op ty1) (fun t0 ->
        MonError.ret (Monadic.AUnaryOp (op, a1', t0))))
  | BNF.ABinaryOp (op, a1, a2) ->
    MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
      MonError.bind (typecheck_atom re gx lx a2) (fun a2' ->
        let ty1 = typof_atom a1' in
        let ty2 = typof_atom a2' in
        MonError.bind (typecheck_binary_op op ty1 ty2) (fun t0 ->
          MonError.ret (Monadic.ABinaryOp (op, a1', a2', t0)))))
  | BNF.ARecordProj (a1, x) ->
    MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
      let ty1 = typof_atom a1' in
      MonError.bind (typecheck_record_proj re ty1 x) (fun t0 ->
        MonError.bind (record_id ty1) (fun _ ->
          MonError.ret (Monadic.ARecordProj (a1', x, t0)))))
  | BNF.ARecordUpdate (a1, x, a2) ->
    MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
      MonError.bind (typecheck_atom re gx lx a2) (fun a2' ->
        let ty1 = typof_atom a1' in
        MonError.bind2 (typecheck_record_update re ty1 a2' x) (fun a2'0 t0 ->
          MonError.bind (record_id ty1) (fun _ ->
            MonError.ret (Monadic.ARecordUpdate (a1', x, a2'0, t0))))))

  (** val typecheck_call_rec :
      Monadic.mtyp list -> Monadic.atom list -> Monadic.mtyp -> (Monadic.atom
      list * Monadic.mtyp) res **)

  let rec typecheck_call_rec tparams args tret =
    match tparams with
    | [] ->
      (match args with
       | [] -> MonError.ret ([], tret)
       | _ :: _ -> MonError.fail)
    | tp1 :: tparams' ->
      (match args with
       | [] -> MonError.fail
       | a1 :: args' ->
         MonError.bind2 (typecheck_call_rec tparams' args' tret)
           (fun args1 t0 ->
           MonError.bind (typecheck_atom_against a1 tp1) (fun a1' ->
             MonError.ret ((a1' :: args1), t0))))

  (** val typecheck_call :
      Monadic.mtyp -> Monadic.atom list -> (Monadic.atom list * Monadic.mtyp)
      res **)

  let typecheck_call ty1 args =
    match ty1 with
    | Monadic.MFun (tparams, tret) -> typecheck_call_rec tparams args tret
    | _ -> MonError.fail

  (** val wrap_mtyp : Monadic.mtyp -> Monadic.mtyp **)

  let rec wrap_mtyp ty = match ty with
  | Monadic.MFun (tparams, tret) ->
    Monadic.MRes (Monadic.MFun (tparams, (wrap_mtyp tret)))
  | Monadic.MRes _ -> ty
  | _ -> Monadic.MRes ty

  (** val unwrap_mtyp : Monadic.mtyp -> Monadic.mtyp **)

  let unwrap_mtyp ty = match ty with
  | Monadic.MRes t0 -> t0
  | _ -> ty

  (** val wrap_atom : Monadic.atom -> Monadic.expr res **)

  let wrap_atom a =
    let ty = typof_atom a in
    let ty' = wrap_mtyp ty in
    MonError.bind
      (match a with
       | Monadic.AVar (x, _) ->
         (match ty with
          | Monadic.MFun (_, _) -> eta_expand x ty (unwrap_mtyp ty')
          | _ -> MonError.ret a)
       | Monadic.ALambda (_, _, _) -> MonError.fail
       | Monadic.ALambdaRet (_, _, _) -> MonError.fail
       | Monadic.AApp (_, _, _) -> MonError.fail
       | _ -> MonError.ret a) (fun a' ->
      MonError.ret (Monadic.ERet ((Monadic.EAtom (a', (typof_atom a'))), ty')))

  (** val monadify_expr_rec :
      archi -> renv -> gcontext -> lcontext -> BNF.expr -> bool ->
      Monadic.expr res **)

  let rec monadify_expr_rec arch re gx lx e imp =
    match e with
    | BNF.EAtom a ->
      MonError.bind (typecheck_atom re gx lx a) (fun a' ->
        if imp
        then (match a' with
              | Monadic.ABinaryOp (b, _, _, _) ->
                (match b with
                 | BopDiv ->
                   (match typof_atom a' with
                    | Monadic.MRes _ ->
                      MonError.ret (Monadic.EAtom (a', (typof_atom a')))
                    | _ -> MonError.fail)
                 | BopMod ->
                   (match typof_atom a' with
                    | Monadic.MRes _ ->
                      MonError.ret (Monadic.EAtom (a', (typof_atom a')))
                    | _ -> MonError.fail)
                 | _ -> wrap_atom a')
              | _ -> wrap_atom a')
        else MonError.ret (Monadic.EAtom (a', (typof_atom a'))))
    | BNF.EArrayGet (a1, a2) ->
      MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
        MonError.bind (typecheck_atom re gx lx a2) (fun a2' ->
          let ty1 = typof_atom a1' in
          let ty2 = typof_atom a2' in
          MonError.bind (typecheck_array_get arch ty1 ty2) (fun t0 ->
            MonError.ret (Monadic.EArrayGet (a1', a2', t0)))))
    | BNF.EArraySet (a1, a2, a3) ->
      MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
        MonError.bind (typecheck_atom re gx lx a2) (fun a2' ->
          MonError.bind (typecheck_atom re gx lx a3) (fun a3' ->
            let ty1 = typof_atom a1' in
            let ty2 = typof_atom a2' in
            MonError.bind2 (typecheck_array_set arch ty1 ty2 a3')
              (fun a3'0 t0 ->
              MonError.ret (Monadic.EArraySet (a1', a2', a3'0, t0))))))
    | BNF.EApp (a1, args) ->
      MonError.bind (typecheck_atom re gx lx a1) (fun a1' ->
        let ty1 = typof_atom a1' in
        MonError.bind (mmap (typecheck_atom re gx lx) args) (fun args' ->
          MonError.bind2 (typecheck_call ty1 args') (fun args1 t0 ->
            let e' = Monadic.EApp (a1', args1, t0) in
            if imp
            then (match t0 with
                  | Monadic.MRes _ -> MonError.ret e'
                  | _ -> MonError.ret (Monadic.ERet (e', (Monadic.MRes t0))))
            else MonError.ret e')))
    | BNF.EIfThenElse (a, e1, e2) ->
      MonError.bind (typecheck_atom re gx lx a) (fun a' ->
        match typof_atom a' with
        | Monadic.MBool ->
          MonError.bind (monadify_expr_rec arch re gx lx e1 imp) (fun e1' ->
            MonError.bind (monadify_expr_rec arch re gx lx e2 imp)
              (fun e2' ->
              let ty1 = typof_expr e1' in
              let ty2 = typof_expr e2' in
              if mtyp_eq_dec ty1 ty2
              then MonError.ret (Monadic.EIfThenElse (a', e1', e2', ty1))
              else (match ty1 with
                    | Monadic.MRes ty1' ->
                      if mtyp_eq_dec ty1' ty2
                      then MonError.bind
                             (monadify_expr_rec arch re gx lx e2 true)
                             (fun e2'0 ->
                             MonError.ret (Monadic.EIfThenElse (a', e1',
                               e2'0, ty1)))
                      else MonError.fail
                    | _ ->
                      (match ty2 with
                       | Monadic.MRes ty2' ->
                         if mtyp_eq_dec ty1 ty2'
                         then MonError.bind
                                (monadify_expr_rec arch re gx lx e1 true)
                                (fun e1'0 ->
                                MonError.ret (Monadic.EIfThenElse (a', e1'0,
                                  e2', ty2)))
                         else MonError.fail
                       | _ -> MonError.fail))))
        | _ -> MonError.fail)
    | BNF.ELetIn (x, e1, e2) ->
      MonError.bind (monadify_expr_rec arch re gx lx e1 false) (fun e1' ->
        let t0 = typof_expr e1' in
        (match t0 with
         | Monadic.MRes tr ->
           MonError.bind (lcontext_update lx x tr) (fun lx' ->
             MonError.bind (monadify_expr_rec arch re gx lx' e2 true)
               (fun e2' ->
               MonError.ret (Monadic.ELetMon (x, e1', e2', (typof_expr e2')))))
         | _ ->
           MonError.bind (lcontext_update lx x t0) (fun lx' ->
             MonError.bind
               (monadify_expr_rec arch re gx lx' e2 ((||) imp false))
               (fun e2' ->
               MonError.ret (Monadic.ELetIn (x, e1', e2', (typof_expr e2')))))))

  (** val monadify_expr :
      archi -> renv -> gcontext -> lcontext -> BNF.expr -> Monadic.expr res **)

  let monadify_expr arch re gx lx e =
    monadify_expr_rec arch re gx lx e false

  (** val monadify_function :
      archi -> renv -> gcontext -> BNF.coq_function -> Monadic.coq_function
      res **)

  let monadify_function arch re gx f =
    let params = SMapList.map monadify_btyp f.fn_params in
    MonError.bind
      (fold_left_err (fun acc pat ->
        let (x, tx) = pat in lcontext_update acc x tx) params
        (MonError.ret STree.empty)) (fun lx ->
      MonError.bind (monadify_expr arch re gx lx f.fn_body) (fun body ->
        MonError.ret { Monadic.fn_return = (typof_expr body);
          Monadic.fn_params = params; Monadic.fn_body = body }))

  (** val make_absfun_tparams :
      (param_attr * btyp) list -> Monadic.mtyp list ->
      (param_attr * Monadic.mtyp) list res **)

  let rec make_absfun_tparams tparams1 tparams2 =
    match tparams1 with
    | [] ->
      (match tparams2 with
       | [] -> MonError.ret []
       | _ :: _ -> MonError.fail)
    | p :: tparams1' ->
      let (attr1, _) = p in
      (match tparams2 with
       | [] -> MonError.fail
       | t2 :: tparams2' ->
         MonError.bind (make_absfun_tparams tparams1' tparams2') (fun r ->
           MonError.ret ((attr1, t2) :: r)))

  (** val typecheck_array_lit : Monadic.literal array -> Monadic.mtyp res **)

  let rec typecheck_array_lit = function
  | [] -> MonError.fail
  | l :: a' ->
    (match a' with
     | [] -> MonError.ret (typof_literal l)
     | _ :: _ ->
       MonError.bind (typecheck_array_lit a') (fun t0 ->
         if mtyp_eq_dec (typof_literal l) t0
         then MonError.ret t0
         else MonError.fail))

  (** val typecheck_struct_lit :
      Monadic.literal SMapList.t -> Monadic.mtyp SMapList.t -> bool **)

  let rec typecheck_struct_lit l1 l2 =
    match l1 with
    | [] -> (match l2 with
             | [] -> true
             | _ :: _ -> false)
    | p :: l1' ->
      let (_, l3) = p in
      (match l2 with
       | [] -> false
       | p0 :: l2' ->
         let (_, tx2) = p0 in
         let tx1 = typof_literal l3 in
         if mtyp_eq_dec tx1 tx2 then typecheck_struct_lit l1' l2' else false)

  (** val typecheck_literal : renv -> BNF.literal -> Monadic.literal res **)

  let rec typecheck_literal re = function
  | LTrue -> MonError.ret (Monadic.LTrue Monadic.MBool)
  | LFalse -> MonError.ret (Monadic.LFalse Monadic.MBool)
  | LInt32 (i, s) -> MonError.ret (Monadic.LInt32 (i, (Monadic.MInt32 s)))
  | LInt64 (i, s) -> MonError.ret (Monadic.LInt64 (i, (Monadic.MInt64 s)))
  | LArray a ->
    MonError.bind (mmap (typecheck_literal re) a) (fun a' ->
      MonError.bind (typecheck_array_lit a') (fun t0 ->
        MonError.ret (Monadic.LArray (a', (Monadic.MArray t0)))))
  | LRecord (rc, rid) ->
    MonError.bind (SMapList.map_err (typecheck_literal re) rc) (fun rc' ->
      MonError.bind (renv_get re rid) (fun t0 ->
        if typecheck_struct_lit rc' t0
        then MonError.ret (Monadic.LRecord (rc', (Monadic.MRecord rid)))
        else MonError.fail))

  (** val monadify_globdefs_rec :
      archi -> renv -> gcontext -> BNF.globdef list -> Monadic.globdef list
      res **)

  let rec monadify_globdefs_rec arch re gx = function
  | [] -> MonError.ret []
  | d :: defs' ->
    (match d with
     | DefConst (x, l, ty) ->
       let ty' = monadify_btyp ty in
       MonError.bind (typecheck_literal re l) (fun l' ->
         if mtyp_eq_dec (typof_literal l') ty'
         then MonError.bind (gcontext_update gx x ty') (fun gx' ->
                MonError.bind (monadify_globdefs_rec arch re gx' defs')
                  (fun r ->
                  MonError.ret ((Monadic.DefConst (x, l', ty')) :: r)))
         else MonError.fail)
     | DefFun (x, f) ->
       MonError.bind (monadify_function arch re gx f) (fun f' ->
         let tf = Monadic.MFun ((map snd f'.Monadic.fn_params),
           f'.Monadic.fn_return)
         in
         MonError.bind (gcontext_update gx x tf) (fun gx' ->
           MonError.bind (monadify_globdefs_rec arch re gx' defs') (fun r ->
             MonError.ret ((Monadic.DefFun (x, f')) :: r))))
     | DeclConst (x, ty) ->
       let ty' = monadify_btyp ty in
       MonError.bind (gcontext_update gx x ty') (fun gx' ->
         MonError.bind (monadify_globdefs_rec arch re gx' defs') (fun r ->
           MonError.ret ((Monadic.DeclConst (x, ty')) :: r)))
     | DeclFun (f, tparams, tret) ->
       let ty' = monadify_btyp (mk_fun_btyp tparams tret) in
       MonError.bind (gcontext_update gx f ty') (fun gx' ->
         MonError.bind (monadify_globdefs_rec arch re gx' defs') (fun r ->
           match ty' with
           | Monadic.MFun (tparams', tret') ->
             MonError.bind (make_absfun_tparams tparams tparams')
               (fun tparams'0 ->
               MonError.ret ((Monadic.DeclFun (f, tparams'0, tret')) :: r))
           | _ -> MonError.fail)))

  (** val monadify_globdefs :
      archi -> renv -> BNF.globdef list -> Monadic.globdef list res **)

  let monadify_globdefs arch re defs =
    monadify_globdefs_rec arch re STree.empty defs

  (** val monadify_program : archi -> BNF.program -> Monadic.program res **)

  let monadify_program arch prog =
    let types =
      map (fun td ->
        match td with
        | TdRecord rc ->
          let fields = SMapList.map monadify_btyp rc.rd_fields in
          Monadic.TdRecord { Monadic.rd_name = rc.rd_name;
          Monadic.rd_fields = fields }
        | TdAbstract (t0, tk) -> Monadic.TdAbstract (t0, tk))
        prog.BNF.prog_types
    in
    MonError.bind
      (fold_left_err (fun acc td ->
        match td with
        | Monadic.TdRecord st ->
          renv_update acc st.Monadic.rd_name st.Monadic.rd_fields
        | Monadic.TdAbstract (_, _) -> MonError.ret acc) types
        (MonError.ret STree.empty)) (fun re ->
      MonError.bind (monadify_globdefs arch re prog.BNF.prog_defs)
        (fun defs ->
        MonError.ret { Monadic.prog_defs = defs; Monadic.prog_types = types }))
 end

(** val monadify_norm_program :
    archi -> Barocq.program -> Monadic.program res **)

let monadify_norm_program arch prog =
  MonError.bind_catch (Normalization.norm_program prog) (fun bnf ->
    MonError.bind_catch (Monadification.monadify_program arch bnf)
      MonError.ret
      ('u'::('n'::('a'::('b'::('l'::('e'::(' '::('t'::('o'::(' '::('m'::('o'::('n'::('a'::('d'::('i'::('f'::('y'::(' '::('t'::('h'::('e'::(' '::('p'::('r'::('o'::('g'::('r'::('a'::('m'::[])))))))))))))))))))))))))))))))
    ('u'::('n'::('a'::('b'::('l'::('e'::(' '::('t'::('o'::(' '::('n'::('o'::('r'::('m'::('a'::('l'::('i'::('z'::('e'::(' '::('t'::('h'::('e'::(' '::('p'::('r'::('o'::('g'::('r'::('a'::('m'::[])))))))))))))))))))))))))))))))
