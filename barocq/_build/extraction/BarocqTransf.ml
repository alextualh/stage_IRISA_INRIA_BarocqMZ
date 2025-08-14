open Barocq
open Datatypes
open Errors
open List0
open Monads
open Syntax
open Target
open Types
open Utils

(** val create_deep_access_expr : BarocqTyped.expr -> expr **)

let rec create_deep_access_expr e =
  let create_deep_access_rec =
    let rec create_deep_access_rec e0 acs =
      match e0 with
      | BarocqTyped.ETrue _ -> (ETrue, acs)
      | BarocqTyped.EFalse _ -> (EFalse, acs)
      | BarocqTyped.EInt32 (i, b) ->
        (match b with
         | BBool -> (ETrue, acs)
         | BInt32 s -> ((EInt32 (i, s)), acs)
         | BInt64 _ -> (ETrue, acs)
         | BArray _ -> (ETrue, acs)
         | BRecord _ -> (ETrue, acs)
         | BFun (_, _) -> (ETrue, acs)
         | BAbs _ -> (ETrue, acs))
      | BarocqTyped.EInt64 (i, b) ->
        (match b with
         | BBool -> (ETrue, acs)
         | BInt32 _ -> (ETrue, acs)
         | BInt64 s -> ((EInt64 (i, s)), acs)
         | BArray _ -> (ETrue, acs)
         | BRecord _ -> (ETrue, acs)
         | BFun (_, _) -> (ETrue, acs)
         | BAbs _ -> (ETrue, acs))
      | BarocqTyped.EVar (x, _) -> ((EVar x), acs)
      | BarocqTyped.ECast (e1, ty) ->
        let e1' = create_deep_access_expr e1 in ((ECast (e1', ty)), acs)
      | BarocqTyped.EUnaryOp (op, e1, _) ->
        let e1' = create_deep_access_expr e1 in ((EUnaryOp (op, e1')), acs)
      | BarocqTyped.EBinaryOp (op, e1, e2, _) ->
        let e1' = create_deep_access_expr e1 in
        let e2' = create_deep_access_expr e2 in
        ((EBinaryOp (op, e1', e2')), acs)
      | BarocqTyped.EArrayGet (e1, e2, ty) ->
        let e2' = create_deep_access_expr e2 in
        if (||) (btyp_is_prim ty) (negb (list_is_empty acs))
        then create_deep_access_rec e1 ((Barocq.AcArrayIndex e2') :: acs)
        else let e1' = create_deep_access_expr e1 in
             ((EArrayGet (e1', e2')), acs)
      | BarocqTyped.EArraySet (e1, e2, e3, _) ->
        let e1' = create_deep_access_expr e1 in
        let e2' = create_deep_access_expr e2 in
        let e3' = create_deep_access_expr e3 in
        ((EArraySet (e1', e2', e3')), acs)
      | BarocqTyped.ERecordProj (e1, f, ty) ->
        if (||) (btyp_is_prim ty) (negb (list_is_empty acs))
        then create_deep_access_rec e1 ((Barocq.AcRecordField f) :: acs)
        else let e1' = create_deep_access_expr e1 in
             ((ERecordProj (e1', f)), acs)
      | BarocqTyped.ERecordUpdate (e1, f, e2, _) ->
        let e1' = create_deep_access_expr e1 in
        let e2' = create_deep_access_expr e2 in
        ((ERecordUpdate (e1', f, e2')), acs)
      | BarocqTyped.EDeepAccess (e1, acs1, ty) ->
        let e1' = create_deep_access_expr e1 in
        let acs1' =
          map (fun ac ->
            match ac with
            | BarocqTyped.AcRecordField (f, _) -> Barocq.AcRecordField f
            | BarocqTyped.AcArrayIndex (ei, _) ->
              let ei' = create_deep_access_expr ei in Barocq.AcArrayIndex ei')
            acs1
        in
        if (||) (btyp_is_prim ty) (negb (list_is_empty acs))
        then (e1', (app acs1' acs))
        else ((EDeepAccess (e1', acs1')), acs)
      | BarocqTyped.EApp (e1, args, _) ->
        let e1' = create_deep_access_expr e1 in
        let args' = map create_deep_access_expr args in
        ((EApp (e1', args')), acs)
      | BarocqTyped.EIfThenElse (e1, e2, e3, _) ->
        let e1' = create_deep_access_expr e1 in
        let e2' = create_deep_access_expr e2 in
        let e3' = create_deep_access_expr e3 in
        ((EIfThenElse (e1', e2', e3')), acs)
      | BarocqTyped.ELetIn (x, e1, e2, _) ->
        let e1' = create_deep_access_expr e1 in
        let e2' = create_deep_access_expr e2 in ((ELetIn (x, e1', e2')), acs)
    in create_deep_access_rec
  in
  let (e', acs) = create_deep_access_rec e [] in
  (match acs with
   | [] -> e'
   | _ :: _ -> EDeepAccess (e', acs))

(** val create_deep_access_function :
    BarocqTyped.coq_function -> Barocq.coq_function **)

let create_deep_access_function f =
  { fn_return = f.fn_return; fn_params = f.fn_params; fn_body =
    (create_deep_access_expr f.fn_body) }

(** val create_deep_access_globdef : BarocqTyped.globdef -> Barocq.globdef **)

let create_deep_access_globdef = function
| BarocqTyped.DefType (s, fields) -> DefType (s, fields)
| BarocqTyped.DefConst (x, l, ty) -> Barocq.DefConst (x, l, ty)
| BarocqTyped.DefFun (x, f) ->
  Barocq.DefFun (x, (create_deep_access_function f))
| BarocqTyped.DeclType (t, tk) -> DeclType (t, tk)
| BarocqTyped.DeclConst (x, ty) -> Barocq.DeclConst (x, ty)
| BarocqTyped.DeclFun (f, tparams, tret) -> Barocq.DeclFun (f, tparams, tret)

(** val create_deep_access_program : BarocqTyped.program -> Barocq.program **)

let create_deep_access_program prog =
  map create_deep_access_globdef prog

(** val transf_program : archi -> Barocq.program -> Barocq.program res **)

let transf_program arch prog =
  MonError.bind (Typing.typecheck_program arch prog) (fun prog0 ->
    MonError.ret (create_deep_access_program prog0))
