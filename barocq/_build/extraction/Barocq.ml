open Barray
open Bool
open Brecord
open Ctypes
open Datatypes
open Errors
open Ident
open Integers
open Intop
open List0
open Maps
open Maps2
open Monads
open Syntax
open Target
open Types
open Typing
open Utils

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type expr =
| ETrue
| EFalse
| EInt32 of Int.int * signedness
| EInt64 of Int64.int * signedness
| EVar of Syntax.ident
| ECast of expr * btyp
| EUnaryOp of unary_op * expr
| EBinaryOp of binary_op * expr * expr
| EArrayGet of expr * expr
| EArraySet of expr * expr * expr
| ERecordProj of expr * Syntax.ident
| ERecordUpdate of expr * Syntax.ident * expr
| EDeepAccess of expr * access list
| EApp of expr * expr list
| EIfThenElse of expr * expr * expr
| ELetIn of Syntax.ident * expr * expr
and access =
| AcRecordField of Syntax.ident
| AcArrayIndex of expr

type coq_function = expr Syntax.coq_function

type globdef =
| DefType of Syntax.ident * (Syntax.ident * btyp) list
| DefConst of Syntax.ident * literal * btyp
| DefFun of Syntax.ident * coq_function
| DeclType of Syntax.ident * struct_or_union
| DeclConst of Syntax.ident * btyp
| DeclFun of Syntax.ident * (param_attr * btyp) list * btyp

type command =
| CmdDef of globdef
| CmdExpr of expr

type program = globdef list

type iprogram = command list

(** val iprog_to_prog : iprogram -> program **)

let iprog_to_prog iprog =
  fold_right (fun cmd acc ->
    match cmd with
    | CmdDef d -> d :: acc
    | CmdExpr _ -> acc) [] iprog

module Typed =
 struct
  type expr =
  | ETrue of btyp
  | EFalse of btyp
  | EInt32 of Int.int * btyp
  | EInt64 of Int64.int * btyp
  | EVar of Syntax.ident * btyp
  | ECast of expr * btyp
  | EUnaryOp of unary_op * expr * btyp
  | EBinaryOp of binary_op * expr * expr * btyp
  | EArrayGet of expr * expr * btyp
  | EArraySet of expr * expr * expr * btyp
  | ERecordProj of expr * Syntax.ident * btyp
  | ERecordUpdate of expr * Syntax.ident * expr * btyp
  | EDeepAccess of expr * access list * btyp
  | EApp of expr * expr list * btyp
  | EIfThenElse of expr * expr * expr * btyp
  | ELetIn of Syntax.ident * expr * expr * btyp
  and access =
  | AcRecordField of Syntax.ident * btyp
  | AcArrayIndex of expr * btyp

  (** val expr_rect :
      (btyp -> 'a1) -> (btyp -> 'a1) -> (Int.int -> btyp -> 'a1) ->
      (Int64.int -> btyp -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (expr ->
      'a1 -> btyp -> 'a1) -> (unary_op -> expr -> 'a1 -> btyp -> 'a1) ->
      (binary_op -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr ->
      'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 ->
      expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> Syntax.ident -> btyp ->
      'a1) -> (expr -> 'a1 -> Syntax.ident -> expr -> 'a1 -> btyp -> 'a1) ->
      (expr -> 'a1 -> access list -> btyp -> 'a1) -> (expr -> 'a1 -> expr
      list -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 ->
      btyp -> 'a1) -> (Syntax.ident -> expr -> 'a1 -> expr -> 'a1 -> btyp ->
      'a1) -> expr -> 'a1 **)

  let rec expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
  | ETrue b -> f b
  | EFalse b -> f0 b
  | EInt32 (i, b) -> f1 i b
  | EInt64 (i, b) -> f2 i b
  | EVar (i, b) -> f3 i b
  | ECast (e0, b) ->
    f4 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) b
  | EUnaryOp (u, e0, b) ->
    f5 u e0
      (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) b
  | EBinaryOp (b, e0, e1, b0) ->
    f6 b e0
      (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) e1
      (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b0
  | EArrayGet (e0, e1, b) ->
    f7 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b
  | EArraySet (e0, e1, e2, b) ->
    f8 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1)
      e2 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e2) b
  | ERecordProj (e0, i, b) ->
    f9 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      i b
  | ERecordUpdate (e0, i, e1, b) ->
    f10 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      i e1 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1)
      b
  | EDeepAccess (e0, l, b) ->
    f11 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      l b
  | EApp (e0, l, b) ->
    f12 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      l b
  | EIfThenElse (e0, e1, e2, b) ->
    f13 e0 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1)
      e2 (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e2) b
  | ELetIn (i, e0, e1, b) ->
    f14 i e0
      (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) e1
      (expr_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b

  (** val expr_rec :
      (btyp -> 'a1) -> (btyp -> 'a1) -> (Int.int -> btyp -> 'a1) ->
      (Int64.int -> btyp -> 'a1) -> (Syntax.ident -> btyp -> 'a1) -> (expr ->
      'a1 -> btyp -> 'a1) -> (unary_op -> expr -> 'a1 -> btyp -> 'a1) ->
      (binary_op -> expr -> 'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr ->
      'a1 -> expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 ->
      expr -> 'a1 -> btyp -> 'a1) -> (expr -> 'a1 -> Syntax.ident -> btyp ->
      'a1) -> (expr -> 'a1 -> Syntax.ident -> expr -> 'a1 -> btyp -> 'a1) ->
      (expr -> 'a1 -> access list -> btyp -> 'a1) -> (expr -> 'a1 -> expr
      list -> btyp -> 'a1) -> (expr -> 'a1 -> expr -> 'a1 -> expr -> 'a1 ->
      btyp -> 'a1) -> (Syntax.ident -> expr -> 'a1 -> expr -> 'a1 -> btyp ->
      'a1) -> expr -> 'a1 **)

  let rec expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 = function
  | ETrue b -> f b
  | EFalse b -> f0 b
  | EInt32 (i, b) -> f1 i b
  | EInt64 (i, b) -> f2 i b
  | EVar (i, b) -> f3 i b
  | ECast (e0, b) ->
    f4 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) b
  | EUnaryOp (u, e0, b) ->
    f5 u e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      b
  | EBinaryOp (b, e0, e1, b0) ->
    f6 b e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b0
  | EArrayGet (e0, e1, b) ->
    f7 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b
  | EArraySet (e0, e1, e2, b) ->
    f8 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) e2
      (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e2) b
  | ERecordProj (e0, i, b) ->
    f9 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) i
      b
  | ERecordUpdate (e0, i, e1, b) ->
    f10 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      i e1 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b
  | EDeepAccess (e0, l, b) ->
    f11 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      l b
  | EApp (e0, l, b) ->
    f12 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      l b
  | EIfThenElse (e0, e1, e2, b) ->
    f13 e0 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0)
      e1 (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) e2
      (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e2) b
  | ELetIn (i, e0, e1, b) ->
    f14 i e0
      (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e0) e1
      (expr_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 e1) b

  (** val access_rect :
      (Syntax.ident -> btyp -> 'a1) -> (expr -> btyp -> 'a1) -> access -> 'a1 **)

  let access_rect f f0 = function
  | AcRecordField (i, b) -> f i b
  | AcArrayIndex (e, b) -> f0 e b

  (** val access_rec :
      (Syntax.ident -> btyp -> 'a1) -> (expr -> btyp -> 'a1) -> access -> 'a1 **)

  let access_rec f f0 = function
  | AcRecordField (i, b) -> f i b
  | AcArrayIndex (e, b) -> f0 e b

  type coq_function = expr Syntax.coq_function

  type globdef =
  | DefType of Syntax.ident * btyp SMapList.t
  | DefConst of Syntax.ident * literal * btyp
  | DefFun of Syntax.ident * coq_function
  | DeclType of Syntax.ident * struct_or_union
  | DeclConst of Syntax.ident * btyp
  | DeclFun of Syntax.ident * (param_attr * btyp) list * btyp

  (** val globdef_rect :
      (Syntax.ident -> btyp SMapList.t -> 'a1) -> (Syntax.ident -> literal ->
      btyp -> 'a1) -> (Syntax.ident -> coq_function -> 'a1) -> (Syntax.ident
      -> struct_or_union -> 'a1) -> (Syntax.ident -> btyp -> 'a1) ->
      (Syntax.ident -> (param_attr * btyp) list -> btyp -> 'a1) -> globdef ->
      'a1 **)

  let globdef_rect f f0 f1 f2 f3 f4 = function
  | DefType (i, t0) -> f i t0
  | DefConst (i, l, b) -> f0 i l b
  | DefFun (i, f5) -> f1 i f5
  | DeclType (i, s) -> f2 i s
  | DeclConst (i, b) -> f3 i b
  | DeclFun (i, l, b) -> f4 i l b

  (** val globdef_rec :
      (Syntax.ident -> btyp SMapList.t -> 'a1) -> (Syntax.ident -> literal ->
      btyp -> 'a1) -> (Syntax.ident -> coq_function -> 'a1) -> (Syntax.ident
      -> struct_or_union -> 'a1) -> (Syntax.ident -> btyp -> 'a1) ->
      (Syntax.ident -> (param_attr * btyp) list -> btyp -> 'a1) -> globdef ->
      'a1 **)

  let globdef_rec f f0 f1 f2 f3 f4 = function
  | DefType (i, t0) -> f i t0
  | DefConst (i, l, b) -> f0 i l b
  | DefFun (i, f5) -> f1 i f5
  | DeclType (i, s) -> f2 i s
  | DeclConst (i, b) -> f3 i b
  | DeclFun (i, l, b) -> f4 i l b

  type program = globdef list
 end

module BarocqTyped = Typed

module Typing =
 struct
  (** val typof_expr : BarocqTyped.expr -> btyp **)

  let typof_expr = function
  | BarocqTyped.ETrue ty -> ty
  | BarocqTyped.EFalse ty -> ty
  | BarocqTyped.EInt32 (_, ty) -> ty
  | BarocqTyped.EInt64 (_, ty) -> ty
  | BarocqTyped.EVar (_, ty) -> ty
  | BarocqTyped.ECast (_, ty) -> ty
  | BarocqTyped.EUnaryOp (_, _, ty) -> ty
  | BarocqTyped.EBinaryOp (_, _, _, ty) -> ty
  | BarocqTyped.EArrayGet (_, _, ty) -> ty
  | BarocqTyped.EArraySet (_, _, _, ty) -> ty
  | BarocqTyped.ERecordProj (_, _, ty) -> ty
  | BarocqTyped.ERecordUpdate (_, _, _, ty) -> ty
  | BarocqTyped.EDeepAccess (_, _, ty) -> ty
  | BarocqTyped.EApp (_, _, ty) -> ty
  | BarocqTyped.EIfThenElse (_, _, _, ty) -> ty
  | BarocqTyped.ELetIn (_, _, _, ty) -> ty

  (** val typecheck_deep_access :
      archi -> (renv -> gcontext -> lcontext -> expr -> BarocqTyped.expr res)
      -> renv -> gcontext -> lcontext -> btyp -> access list ->
      (btyp * Typed.access list) res **)

  let rec typecheck_deep_access arch typecheck_expr0 re gx lx ty = function
  | [] -> MonError.ret (ty, [])
  | ac :: acs' ->
    (match ac with
     | AcRecordField f ->
       MonError.bind (typecheck_record_proj re ty f) (fun ty' ->
         MonError.bind2
           (typecheck_deep_access arch typecheck_expr0 re gx lx ty' acs')
           (fun r lr ->
           MonError.ret (r, ((BarocqTyped.AcRecordField (f, ty')) :: lr))))
     | AcArrayIndex ei ->
       MonError.bind (typecheck_expr0 re gx lx ei) (fun ei' ->
         MonError.bind (typecheck_array_get arch ty (typof_expr ei'))
           (fun ty' ->
           MonError.bind2
             (typecheck_deep_access arch typecheck_expr0 re gx lx ty' acs')
             (fun r lr ->
             MonError.ret (r, ((BarocqTyped.AcArrayIndex (ei', ty')) :: lr))))))

  (** val typecheck_expr :
      archi -> renv -> gcontext -> lcontext -> expr -> BarocqTyped.expr res **)

  let rec typecheck_expr arch re gx lx = function
  | ETrue -> MonError.ret (BarocqTyped.ETrue BBool)
  | EFalse -> MonError.ret (BarocqTyped.EFalse BBool)
  | EInt32 (i, s) -> MonError.ret (BarocqTyped.EInt32 (i, (BInt32 s)))
  | EInt64 (i, s) -> MonError.ret (BarocqTyped.EInt64 (i, (BInt64 s)))
  | EVar x ->
    MonError.bind (typof_var gx lx x) (fun t0 ->
      MonError.ret (BarocqTyped.EVar (x, t0)))
  | ECast (e1, ty) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_cast (typof_expr e1') ty) (fun t0 ->
        MonError.ret (BarocqTyped.ECast (e1', t0))))
  | EUnaryOp (op, e1) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_unary_op op (typof_expr e1')) (fun t0 ->
        MonError.ret (BarocqTyped.EUnaryOp (op, e1', t0))))
  | EBinaryOp (op, e1, e2) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_expr arch re gx lx e2) (fun e2' ->
        MonError.bind
          (typecheck_binary_op op (typof_expr e1') (typof_expr e2'))
          (fun t0 -> MonError.ret (BarocqTyped.EBinaryOp (op, e1', e2', t0)))))
  | EArrayGet (e1, e2) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_expr arch re gx lx e2) (fun e2' ->
        MonError.bind
          (typecheck_array_get arch (typof_expr e1') (typof_expr e2'))
          (fun t0 -> MonError.ret (BarocqTyped.EArrayGet (e1', e2', t0)))))
  | EArraySet (e1, e2, e3) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_expr arch re gx lx e2) (fun e2' ->
        MonError.bind (typecheck_expr arch re gx lx e3) (fun e3' ->
          MonError.bind
            (typecheck_array_set arch (typof_expr e1') (typof_expr e2')
              (typof_expr e3')) (fun t0 ->
            MonError.ret (BarocqTyped.EArraySet (e1', e2', e3', t0))))))
  | ERecordProj (e1, x) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_record_proj re (typof_expr e1') x) (fun t0 ->
        MonError.ret (BarocqTyped.ERecordProj (e1', x, t0))))
  | ERecordUpdate (e1, x, e2) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_expr arch re gx lx e2) (fun e2' ->
        MonError.bind
          (typecheck_record_update re (typof_expr e1') (typof_expr e2') x)
          (fun t0 ->
          MonError.ret (BarocqTyped.ERecordUpdate (e1', x, e2', t0)))))
  | EDeepAccess (e1, acs) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind2
        (typecheck_deep_access arch (typecheck_expr arch) re gx lx
          (typof_expr e1') acs) (fun t0 acs' ->
        MonError.ret (BarocqTyped.EDeepAccess (e1', acs', t0))))
  | EApp (e1, args) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (mmap (typecheck_expr arch re gx lx) args) (fun args' ->
        let targs = map typof_expr args' in
        MonError.bind (typecheck_call (typof_expr e1') targs) (fun t0 ->
          MonError.ret (BarocqTyped.EApp (e1', args', t0)))))
  | EIfThenElse (e1, e2, e3) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (typecheck_expr arch re gx lx e2) (fun e2' ->
        MonError.bind (typecheck_expr arch re gx lx e3) (fun e3' ->
          let p = ((typof_expr e1'), (typof_expr e2')) in
          let ty3 = typof_expr e3' in
          let (ty1, ty2) = p in
          (match ty1 with
           | BBool ->
             if btyp_eq_dec ty2 ty3
             then MonError.ret (BarocqTyped.EIfThenElse (e1', e2', e3', ty2))
             else MonError.fail
           | _ -> MonError.fail))))
  | ELetIn (x, e1, e2) ->
    MonError.bind (typecheck_expr arch re gx lx e1) (fun e1' ->
      MonError.bind (lcontext_update lx x (typof_expr e1')) (fun lx' ->
        MonError.bind (typecheck_expr arch re gx lx' e2) (fun e2' ->
          MonError.ret (BarocqTyped.ELetIn (x, e1', e2', (typof_expr e2'))))))

  (** val typecheck_function :
      archi -> archi -> renv -> gcontext -> coq_function ->
      BarocqTyped.coq_function res **)

  let typecheck_function arch _ re gx f =
    MonError.bind
      (fold_left_err (fun acc pat ->
        let (x, tx) = pat in lcontext_update acc x tx) f.fn_params
        (MonError.ret STree.empty)) (fun lx ->
      MonError.bind (typecheck_expr arch re gx lx f.fn_body) (fun body ->
        if btyp_eq_dec (typof_expr body) f.fn_return
        then MonError.ret { fn_return = f.fn_return; fn_params = f.fn_params;
               fn_body = body }
        else MonError.failwith
               ('B'::('a'::('r'::('o'::('c'::('q'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(':'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  (** val typecheck_globdefs :
      archi -> renv -> gcontext -> globdef list -> BarocqTyped.globdef list
      res **)

  let rec typecheck_globdefs arch re gx = function
  | [] -> MonError.ret []
  | d :: defs' ->
    (match d with
     | DefType (x, fields) ->
       MonError.bind (renv_update re x fields) (fun re' ->
         MonError.bind (typecheck_globdefs arch re' gx defs') (fun rd ->
           MonError.ret ((BarocqTyped.DefType (x, fields)) :: rd)))
     | DefConst (x, l, ty) ->
       MonError.bind (typecheck_literal re l) (fun l' ->
         if btyp_eq_dec ty (typof_literal l')
         then MonError.bind (gcontext_update gx x ty) (fun gx' ->
                MonError.bind (typecheck_globdefs arch re gx' defs')
                  (fun rd ->
                  MonError.ret ((BarocqTyped.DefConst (x, l, ty)) :: rd)))
         else MonError.failwith
                ('B'::('a'::('r'::('o'::('c'::('q'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('g'::('l'::('o'::('b'::('d'::('e'::('f'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(' '::('d'::('e'::('f'::('i'::('n'::('i'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | DefFun (x, f) ->
       MonError.bind (typecheck_function arch arch re gx f) (fun f' ->
         let tf = mk_fun_btyp f'.fn_params f'.fn_return in
         MonError.bind (gcontext_update gx x tf) (fun gx' ->
           MonError.bind (typecheck_globdefs arch re gx' defs') (fun rd ->
             MonError.ret ((BarocqTyped.DefFun (x, f')) :: rd))))
     | DeclType (t0, tk) ->
       MonError.bind (typecheck_globdefs arch re gx defs') (fun rd ->
         MonError.ret ((BarocqTyped.DeclType (t0, tk)) :: rd))
     | DeclConst (x, ty) ->
       MonError.bind (gcontext_update gx x ty) (fun gx' ->
         MonError.bind (typecheck_globdefs arch re gx' defs') (fun rd ->
           MonError.ret ((BarocqTyped.DeclConst (x, ty)) :: rd)))
     | DeclFun (x, tparams, tret) ->
       let tf = mk_fun_btyp tparams tret in
       MonError.bind (gcontext_update gx x tf) (fun gx' ->
         MonError.bind (typecheck_globdefs arch re gx' defs') (fun rd ->
           MonError.ret ((BarocqTyped.DeclFun (x, tparams, tret)) :: rd))))

  (** val typecheck_program : archi -> program -> BarocqTyped.program res **)

  let typecheck_program arch prog =
    typecheck_globdefs arch STree.empty STree.empty prog
 end

type value =
| Val of typ * eval_typ

type access_value =
| AcvalRecordField of Syntax.ident
| AcvalArrayIndex of value

type genv = value STree.t

type lenv = value STree.t

(** val genv_get : __ PMap.t -> genv -> Syntax.ident -> value res **)

let genv_get _ ge x =
  MonError.err_of_opt (STree.get x ge)

(** val genv_update :
    __ PMap.t -> genv -> Syntax.ident -> value -> genv res **)

let genv_update abs_typ_impl ge x v =
  match genv_get abs_typ_impl ge x with
  | OK _ -> MonError.fail
  | Error _ -> MonError.ret (STree.set x v ge)

(** val lenv_get : __ PMap.t -> lenv -> Syntax.ident -> value res **)

let lenv_get _ le x =
  MonError.err_of_opt (STree.get x le)

(** val lenv_update : __ PMap.t -> lenv -> Syntax.ident -> value -> lenv **)

let lenv_update _ le x v =
  STree.set x v le

(** val eval_var : __ PMap.t -> genv -> lenv -> Syntax.ident -> value res **)

let eval_var abs_typ_impl ge le x =
  match lenv_get abs_typ_impl le x with
  | OK v -> MonError.ret v
  | Error _ -> genv_get abs_typ_impl ge x

(** val eval_cast : __ PMap.t -> value -> typ -> value res **)

let eval_cast _ v to0 =
  let Val (t0, i) = v in
  (match t0 with
   | TBool ->
     (match to0 with
      | TBool -> MonError.ret (Val (TBool, i))
      | TInt32 s ->
        let iv =
          match s with
          | Signed -> I32.of_bool (Obj.magic i)
          | Unsigned -> U32.of_bool (Obj.magic i)
        in
        MonError.ret (Val ((TInt32 s), (Obj.magic iv)))
      | TInt64 s ->
        let iv =
          match s with
          | Signed -> I64.of_bool (Obj.magic i)
          | Unsigned -> U64.of_bool (Obj.magic i)
        in
        MonError.ret (Val ((TInt64 s), (Obj.magic iv)))
      | _ -> MonError.fail)
   | TInt32 s ->
     (match to0 with
      | TBool ->
        (match s with
         | Signed -> MonError.ret (Val (TBool, (Obj.magic I32.to_bool i)))
         | Unsigned -> MonError.ret (Val (TBool, (Obj.magic U32.to_bool i))))
      | TInt32 s' ->
        (match s with
         | Signed ->
           (match s' with
            | Signed -> MonError.ret (Val ((TInt32 s'), i))
            | Unsigned ->
              MonError.ret (Val ((TInt32 s'), (Obj.magic U32.of_i32 i))))
         | Unsigned ->
           (match s' with
            | Signed ->
              MonError.ret (Val ((TInt32 s'), (Obj.magic I32.of_u32 i)))
            | Unsigned -> MonError.ret (Val ((TInt32 s'), i))))
      | TInt64 s' ->
        (match s with
         | Signed ->
           (match s' with
            | Signed ->
              MonError.ret (Val ((TInt64 s'), (Obj.magic I64.of_i32 i)))
            | Unsigned ->
              MonError.ret (Val ((TInt64 s'), (Obj.magic U64.of_i32 i))))
         | Unsigned ->
           (match s' with
            | Signed ->
              MonError.ret (Val ((TInt64 s'), (Obj.magic I64.of_u32 i)))
            | Unsigned ->
              MonError.ret (Val ((TInt64 s'), (Obj.magic U64.of_u32 i)))))
      | _ -> MonError.fail)
   | TInt64 s ->
     (match to0 with
      | TBool ->
        (match s with
         | Signed -> MonError.ret (Val (TBool, (Obj.magic I64.to_bool i)))
         | Unsigned -> MonError.ret (Val (TBool, (Obj.magic U64.to_bool i))))
      | TInt32 s' ->
        (match s with
         | Signed ->
           (match s' with
            | Signed ->
              MonError.ret (Val ((TInt32 s'), (Obj.magic I32.of_i64 i)))
            | Unsigned ->
              MonError.ret (Val ((TInt32 s'), (Obj.magic U32.of_i64 i))))
         | Unsigned ->
           (match s' with
            | Signed ->
              MonError.ret (Val ((TInt32 s'), (Obj.magic I32.of_u64 i)))
            | Unsigned ->
              MonError.ret (Val ((TInt32 s'), (Obj.magic U32.of_u64 i)))))
      | TInt64 s' ->
        (match s with
         | Signed ->
           (match s' with
            | Signed -> MonError.ret (Val ((TInt64 s'), i))
            | Unsigned ->
              MonError.ret (Val ((TInt64 s'), (Obj.magic U64.of_i64 i))))
         | Unsigned ->
           (match s' with
            | Signed ->
              MonError.ret (Val ((TInt64 s'), (Obj.magic I64.of_u64 i)))
            | Unsigned -> MonError.ret (Val ((TInt64 s'), i))))
      | _ -> MonError.fail)
   | _ -> MonError.fail)

(** val eval_unary_op : __ PMap.t -> unary_op -> value -> value res **)

let eval_unary_op _ op v =
  match op with
  | UopNotbool ->
    let Val (t0, b) = v in
    (match t0 with
     | TBool -> MonError.ret (Val (TBool, (Obj.magic negb b)))
     | _ -> MonError.fail)
  | UopNotint ->
    let Val (t0, i) = v in
    (match t0 with
     | TInt32 s -> MonError.ret (Val ((TInt32 s), (Obj.magic Int.not i)))
     | TInt64 s -> MonError.ret (Val ((TInt64 s), (Obj.magic Int64.not i)))
     | _ -> MonError.fail)
  | UopNeg ->
    let Val (t0, i) = v in
    (match t0 with
     | TInt32 s -> MonError.ret (Val ((TInt32 s), (Obj.magic Int.neg i)))
     | TInt64 s -> MonError.ret (Val ((TInt64 s), (Obj.magic Int64.neg i)))
     | _ -> MonError.fail)
  | UopPlus ->
    let Val (t0, i) = v in
    (match t0 with
     | TInt32 s -> MonError.ret (Val ((TInt32 s), i))
     | TInt64 s -> MonError.ret (Val ((TInt64 s), i))
     | _ -> MonError.fail)

(** val eval_binary_op :
    __ PMap.t -> binary_op -> value -> value -> value res **)

let eval_binary_op _ op v1 v2 =
  match op with
  | BopAndbool ->
    let Val (t0, b1) = v1 in
    (match t0 with
     | TBool ->
       let Val (t1, b2) = v2 in
       (match t1 with
        | TBool -> MonError.ret (Val (TBool, (Obj.magic (&&) b1 b2)))
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopOrbool ->
    let Val (t0, b1) = v1 in
    (match t0 with
     | TBool ->
       let Val (t1, b2) = v2 in
       (match t1 with
        | TBool -> MonError.ret (Val (TBool, (Obj.magic (||) b1 b2)))
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopXorbool ->
    let Val (t0, b1) = v1 in
    (match t0 with
     | TBool ->
       let Val (t1, b2) = v2 in
       (match t1 with
        | TBool -> MonError.ret (Val (TBool, (Obj.magic xorb b1 b2)))
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopAdd ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.add i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1), (Obj.magic Int64.add i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopSub ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.sub i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1), (Obj.magic Int64.sub i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopMul ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.mul i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1), (Obj.magic Int64.mul i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopDiv ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed ->
                MonError.bind (Obj.magic I32.div i1 i2) (fun r ->
                  MonError.ret (Val ((TInt32 Signed), r)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.bind (Obj.magic U32.div i1 i2) (fun r ->
                  MonError.ret (Val ((TInt32 Unsigned), r))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.bind (Obj.magic I64.div i1 i2) (fun r ->
                  MonError.ret (Val ((TInt64 Signed), r)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.bind (Obj.magic U64.div i1 i2) (fun r ->
                  MonError.ret (Val ((TInt64 Unsigned), r))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)
  | BopMod ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed ->
                MonError.bind (Obj.magic I32.coq_mod i1 i2) (fun r ->
                  MonError.ret (Val ((TInt32 Signed), r)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.bind (U32.coq_mod (Obj.magic i1) (Obj.magic i2))
                  (fun _ ->
                  MonError.ret (Val ((TInt32 Unsigned),
                    (Obj.magic Int.modu i1 i2)))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.bind (I64.coq_mod (Obj.magic i1) (Obj.magic i2))
                  (fun _ ->
                  MonError.ret (Val ((TInt64 Signed),
                    (Obj.magic Int64.mods i1 i2))))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.bind (Obj.magic U64.coq_mod i1 i2) (fun r ->
                  MonError.ret (Val ((TInt64 Unsigned), r))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)
  | BopAndint ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.coq_and i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1),
                 (Obj.magic Int64.coq_and i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopOrint ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.coq_or i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1),
                 (Obj.magic Int64.coq_or i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopXorint ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.xor i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1), (Obj.magic Int64.xor i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopShl ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt32 s1), (Obj.magic Int.shl i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val ((TInt64 s1), (Obj.magic Int64.shl i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopShr ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val ((TInt32 Signed),
                  (Obj.magic Int.shr i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val ((TInt32 Unsigned),
                  (Obj.magic Int.shru i1 i2))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val ((TInt64 Signed),
                  (Obj.magic Int64.shr i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val ((TInt64 Unsigned),
                  (Obj.magic Int64.shru i1 i2))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)
  | BopEq ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TBool ->
       let Val (t1, b2) = v2 in
       (match t1 with
        | TBool -> MonError.ret (Val (TBool, (Obj.magic eqb i1 b2)))
        | _ -> MonError.fail)
     | TInt32 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val (TBool, (Obj.magic Int.eq i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | TInt64 s1 ->
       let Val (t1, i2) = v2 in
       (match t1 with
        | TInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret (Val (TBool, (Obj.magic Int64.eq i1 i2)))
          else MonError.fail
        | _ -> MonError.fail)
     | _ -> MonError.fail)
  | BopNeq ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TBool ->
       let Val (t1, b2) = v2 in
       (match t1 with
        | TBool ->
          MonError.ret (Val (TBool,
            (Obj.magic negb (eqb (Obj.magic i1) (Obj.magic b2)))))
        | _ -> MonError.fail)
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int.cmp Cne i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int.cmpu Cne i1 i2))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int64.cmp Cne i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int64.cmpu Cne i1 i2))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)
  | BopLt ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.ret (Val (TBool, (Obj.magic Int.lt i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int.ltu i1 i2))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int64.lt i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int64.ltu i1 i2))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)
  | BopLe ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int.cmp Cle i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int.cmpu Cle i1 i2))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int64.cmp Cle i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int64.cmpu Cle i1 i2))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)
  | _ ->
    let Val (t0, i1) = v1 in
    (match t0 with
     | TInt32 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int.cmp Cge i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt32 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int.cmpu Cge i1 i2))))
           | _ -> MonError.fail))
     | TInt64 s ->
       (match s with
        | Signed ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed ->
                MonError.ret (Val (TBool, (Obj.magic Int64.cmp Cge i1 i2)))
              | Unsigned -> MonError.fail)
           | _ -> MonError.fail)
        | Unsigned ->
          let Val (t1, i2) = v2 in
          (match t1 with
           | TInt64 s0 ->
             (match s0 with
              | Signed -> MonError.fail
              | Unsigned ->
                MonError.ret (Val (TBool, (Obj.magic Int64.cmpu Cge i1 i2))))
           | _ -> MonError.fail))
     | _ -> MonError.fail)

(** val eval_array_lit : __ PMap.t -> value array -> value res **)

let rec eval_array_lit abs_typ_impl = function
| [] -> MonError.fail
| v :: a' ->
  let Val (tx, x) = v in
  (match a' with
   | [] -> MonError.ret (Val ((TArray tx), (Obj.magic (x :: []))))
   | _ :: _ ->
     MonError.bind (eval_array_lit abs_typ_impl a') (fun va ->
       let Val (t0, xa) = va in
       (match t0 with
        | TArray ta ->
          if typ_eq_dec tx ta
          then MonError.ret (Val ((TArray ta),
                 (Obj.magic
                   ((typ_cast tx ta abs_typ_impl x) :: (Obj.magic xa)))))
          else MonError.fail
        | _ -> MonError.fail)))

(** val eval_record_lit_rec :
    __ PMap.t -> value SMapList.t -> typ SMapList.t -> eval_typ
    eval_recordtyp res **)

let rec eval_record_lit_rec abs_typ_impl lv fields =
  match lv with
  | [] ->
    (match fields with
     | [] -> MonError.ret (Obj.magic ())
     | _ :: _ -> MonError.fail)
  | p :: l ->
    let (k, v) = p in
    let Val (t0, v0) = v in
    (match fields with
     | [] -> MonError.fail
     | p0 :: l0 ->
       let (k0, t1) = p0 in
       let s = eq_dec k k0 in
       if s
       then let s0 = typ_eq_dec t0 t1 in
            if s0
            then let r = eval_record_lit_rec abs_typ_impl l l0 in
                 (match r with
                  | OK e -> MonError.ret (Obj.magic (v0, e))
                  | Error _ -> MonError.fail)
            else MonError.fail
       else MonError.fail)

(** val eval_record_lit :
    __ PMap.t -> Syntax.ident -> value SMapList.t -> typ SMapList.t -> value
    res **)

let eval_record_lit abs_typ_impl n lv fields =
  match lv with
  | [] -> MonError.fail
  | p :: l ->
    let r = eval_record_lit_rec abs_typ_impl (p :: l) fields in
    (match r with
     | OK e -> MonError.ret (Val ((TRecord (n, fields)), e))
     | Error _ -> MonError.fail)

(** val eval_array_get : archi -> __ PMap.t -> value -> value -> value res **)

let eval_array_get arch _ v1 v2 =
  let Val (t0, v) = v1 in
  let Val (t1, v0) = v2 in
  (match t0 with
   | TArray t2 ->
     (match arch with
      | Ptr32 ->
        let s = typ_eq_dec t1 (TInt32 Unsigned) in
        if s
        then let r = get (Obj.magic v) (U64.of_u32 (Obj.magic v0)) in
             (match r with
              | OK e -> MonError.ret (Val (t2, e))
              | Error _ -> MonError.fail)
        else MonError.fail
      | Ptr64 ->
        let s = typ_eq_dec t1 (TInt64 Unsigned) in
        if s
        then let r = get (Obj.magic v) (Obj.magic v0) in
             (match r with
              | OK e -> MonError.ret (Val (t2, e))
              | Error _ -> MonError.fail)
        else MonError.fail)
   | _ -> MonError.fail)

(** val eval_array_set :
    archi -> __ PMap.t -> value -> value -> value -> value res **)

let eval_array_set arch _ v1 v2 v3 =
  let Val (t0, v) = v1 in
  let Val (t1, v0) = v2 in
  let Val (t2, v4) = v3 in
  (match t0 with
   | TArray t3 ->
     (match arch with
      | Ptr32 ->
        let s = typ_eq_dec t1 (TInt32 Unsigned) in
        if s
        then let s0 = typ_eq_dec t3 t2 in
             if s0
             then let r = set (Obj.magic v) (U64.of_u32 (Obj.magic v0)) v4 in
                  (match r with
                   | OK a -> MonError.ret (Val ((TArray t2), (Obj.magic a)))
                   | Error _ -> MonError.fail)
             else MonError.fail
        else MonError.fail
      | Ptr64 ->
        let s = typ_eq_dec t1 (TInt64 Unsigned) in
        if s
        then let s0 = typ_eq_dec t3 t2 in
             if s0
             then let r = set (Obj.magic v) (Obj.magic v0) v4 in
                  (match r with
                   | OK a -> MonError.ret (Val ((TArray t2), (Obj.magic a)))
                   | Error _ -> MonError.fail)
             else MonError.fail
        else MonError.fail)
   | _ -> MonError.fail)

(** val proj_field : key -> 'a1 field -> 'a1 **)

let proj_field _ fd =
  fd

(** val eval_record_proj_aux :
    __ PMap.t -> typ SMapList.t -> eval_typ eval_recordtyp -> Syntax.ident ->
    value res **)

let rec eval_record_proj_aux abs_typ_impl fields rc k =
  match fields with
  | [] -> MonError.fail
  | p :: l ->
    let (k0, t0) = p in
    let s = SMapList.key_eq k0 k in
    if s
    then OK (Val (t0, (proj_field (fst (k0, __)) (fst (Obj.magic rc)))))
    else eval_record_proj_aux abs_typ_impl l (snd (Obj.magic rc)) k

(** val eval_record_proj : __ PMap.t -> value -> Syntax.ident -> value res **)

let eval_record_proj abs_typ_impl v k =
  let Val (t0, st) = v in
  (match t0 with
   | TRecord (_, fields) -> eval_record_proj_aux abs_typ_impl fields st k
   | _ -> MonError.fail)

(** val typof_field_dec : Syntax.ident -> typ SMapList.t -> typ res **)

let typof_field_dec k fields =
  let r = typof_field k fields in
  (match r with
   | OK t0 -> OK t0
   | Error _ -> MonError.fail)

(** val cast_typof_field :
    __ PMap.t -> Syntax.ident -> typ SMapList.t -> typ -> eval_typ ->
    type_of_field res **)

let cast_typof_field _ k fields tv v =
  let r = typof_field_dec k fields in
  (match r with
   | OK s -> let s0 = typ_eq_dec tv s in if s0 then OK v else MonError.fail
   | Error _ -> MonError.fail)

(** val eval_record_update_aux :
    __ PMap.t -> Syntax.ident -> typ SMapList.t -> eval_typ -> Syntax.ident
    -> value -> value res **)

let eval_record_update_aux abs_typ_impl n fields rc k = function
| Val (t0, v0) ->
  let v1 = cast_typof_field abs_typ_impl k fields t0 v0 in
  (match v1 with
   | OK t1 ->
     let r = update (eval_fields_typ fields) rc k t1 in
     (match r with
      | OK r0 -> MonError.ret (Val ((TRecord (n, fields)), r0))
      | Error _ -> MonError.fail)
   | Error _ -> MonError.fail)

(** val eval_record_update :
    __ PMap.t -> value -> Syntax.ident -> value -> value res **)

let eval_record_update abs_typ_impl v1 k v2 =
  let Val (t0, st) = v1 in
  (match t0 with
   | TRecord (n, fields) ->
     eval_record_update_aux abs_typ_impl n fields st k v2
   | _ -> MonError.fail)

(** val eval_access_list :
    archi -> __ PMap.t -> value -> access_value list -> value res **)

let rec eval_access_list arch abs_typ_impl v = function
| [] -> OK v
| ac :: acs' ->
  (match ac with
   | AcvalRecordField f ->
     MonError.bind (eval_record_proj abs_typ_impl v f) (fun v' ->
       eval_access_list arch abs_typ_impl v' acs')
   | AcvalArrayIndex va ->
     MonError.bind (eval_array_get arch abs_typ_impl v va) (fun v' ->
       eval_access_list arch abs_typ_impl v' acs'))

(** val eval_ifthenelse :
    __ PMap.t -> value -> value res -> value res -> value res **)

let eval_ifthenelse _ v1 v2 v3 =
  let Val (t0, b) = v1 in
  (match t0 with
   | TBool -> if Obj.magic b then v2 else v3
   | _ -> MonError.fail)

(** val eval_app_rec :
    __ PMap.t -> typ list -> typ -> eval_typ eval_funtyp -> value list ->
    value res **)

let rec eval_app_rec abs_typ_impl tparams tret f args =
  match tparams with
  | [] ->
    (match args with
     | [] ->
       MonError.bind (Obj.magic f) (fun v -> MonError.ret (Val (tret, v)))
     | _ :: _ -> MonError.fail)
  | t0 :: l ->
    (match args with
     | [] -> MonError.fail
     | v :: l0 ->
       let Val (t1, v0) = v in
       if typ_eq_dec t1 t0
       then eval_app_rec abs_typ_impl l tret
              (Obj.magic f (typ_cast t1 t0 abs_typ_impl v0)) l0
       else MonError.fail)

(** val eval_app : __ PMap.t -> value -> value list -> value res **)

let eval_app abs_typ_impl v args =
  let Val (t0, v0) = v in
  (match t0 with
   | TFun (tparams, tret) ->
     (match tparams with
      | [] ->
        (match args with
         | [] ->
           (match Obj.magic v0 () with
            | OK e -> MonError.ret (Val (tret, e))
            | Error e -> Error e)
         | _ :: _ -> MonError.fail)
      | t1 :: l -> eval_app_rec abs_typ_impl (t1 :: l) tret v0 args)
   | _ -> MonError.fail)

(** val eval_expr :
    archi -> __ PMap.t -> tenv -> genv -> lenv -> expr -> value res **)

let eval_expr arch abs_typ_impl =
  let rec eval_expr0 te ge le = function
  | ETrue -> MonError.ret (Val (TBool, (Obj.magic true)))
  | EFalse -> MonError.ret (Val (TBool, (Obj.magic false)))
  | EInt32 (i, s) -> MonError.ret (Val ((TInt32 s), (Obj.magic i)))
  | EInt64 (i, s) -> MonError.ret (Val ((TInt64 s), (Obj.magic i)))
  | EVar x -> eval_var abs_typ_impl ge le x
  | ECast (e1, ty) ->
    MonError.bind (btyp_to_typ te ty) (fun ty' ->
      MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
        eval_cast abs_typ_impl v1 ty'))
  | EUnaryOp (op, e0) ->
    MonError.bind (eval_expr0 te ge le e0) (fun v ->
      eval_unary_op abs_typ_impl op v)
  | EBinaryOp (op, e1, e2) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      MonError.bind (eval_expr0 te ge le e2) (fun v2 ->
        eval_binary_op abs_typ_impl op v1 v2))
  | EArrayGet (e1, e2) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      MonError.bind (eval_expr0 te ge le e2) (fun v2 ->
        eval_array_get arch abs_typ_impl v1 v2))
  | EArraySet (e1, e2, e3) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      MonError.bind (eval_expr0 te ge le e2) (fun v2 ->
        MonError.bind (eval_expr0 te ge le e3) (fun v3 ->
          eval_array_set arch abs_typ_impl v1 v2 v3)))
  | ERecordProj (e0, k) ->
    MonError.bind (eval_expr0 te ge le e0) (fun v ->
      eval_record_proj abs_typ_impl v k)
  | ERecordUpdate (e1, k, e2) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      MonError.bind (eval_expr0 te ge le e2) (fun v2 ->
        eval_record_update abs_typ_impl v1 k v2))
  | EDeepAccess (e1, acs) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      MonError.bind (mmap (eval_access_expr te ge le) acs) (fun vacs ->
        eval_access_list arch abs_typ_impl v1 vacs))
  | EApp (v, args) ->
    MonError.bind (eval_expr0 te ge le v) (fun f ->
      MonError.bind (mmap (eval_expr0 te ge le) args) (fun vargs ->
        eval_app abs_typ_impl f vargs))
  | EIfThenElse (e1, e2, e3) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      let v2 = eval_expr0 te ge le e2 in
      let v3 = eval_expr0 te ge le e3 in eval_ifthenelse abs_typ_impl v1 v2 v3)
  | ELetIn (x, e1, e2) ->
    MonError.bind (eval_expr0 te ge le e1) (fun v1 ->
      let le' = lenv_update abs_typ_impl le x v1 in eval_expr0 te ge le' e2)
  and eval_access_expr te ge le = function
  | AcRecordField f -> MonError.ret (AcvalRecordField f)
  | AcArrayIndex e ->
    MonError.bind (eval_expr0 te ge le e) (fun v ->
      MonError.ret (AcvalArrayIndex v))
  in eval_expr0

(** val eval_literal : __ PMap.t -> tenv -> literal -> value res **)

let rec eval_literal abs_typ_impl te = function
| LTrue -> MonError.ret (Val (TBool, (Obj.magic true)))
| LFalse -> MonError.ret (Val (TBool, (Obj.magic false)))
| LInt32 (i, s) -> MonError.ret (Val ((TInt32 s), (Obj.magic i)))
| LInt64 (i, s) -> MonError.ret (Val ((TInt64 s), (Obj.magic i)))
| LArray a ->
  MonError.bind (mmap (eval_literal abs_typ_impl te) a) (fun av ->
    eval_array_lit abs_typ_impl av)
| LRecord (rc, x) ->
  MonError.bind (SMapList.map_err (eval_literal abs_typ_impl te) rc)
    (fun rcv ->
    MonError.bind (tenv_get te x) (fun fields ->
      eval_record_lit abs_typ_impl x rcv fields))

(** val cast_typ_M : __ PMap.t -> typ -> value -> eval_typ MonError.coq_M **)

let cast_typ_M abs_typ_impl tret = function
| Val (tv, v0) ->
  if typ_eq_dec tv tret
  then MonError.ret (typ_cast tv tret abs_typ_impl v0)
  else MonError.fail

(** val build_funval_rec :
    archi -> __ PMap.t -> tenv -> genv -> lenv -> typ SMapList.t -> typ ->
    expr -> eval_typ eval_funtyp **)

let rec build_funval_rec arch abs_typ_impl te ge le params tret e =
  match params with
  | [] ->
    Obj.magic MonError.bind (eval_expr arch abs_typ_impl te ge le e)
      (fun v -> cast_typ_M abs_typ_impl tret v)
  | p :: l ->
    let (k, t0) = p in
    Obj.magic (fun y ->
      build_funval_rec arch abs_typ_impl te ge
        (lenv_update abs_typ_impl le k (Val (t0, y))) l tret e)

(** val build_funval :
    archi -> __ PMap.t -> tenv -> genv -> typ SMapList.t -> typ -> expr ->
    eval_typ **)

let build_funval arch abs_typ_impl te ge params tret e =
  match params with
  | [] ->
    Obj.magic (fun _ ->
      MonError.bind (eval_expr arch abs_typ_impl te ge STree.empty e)
        (fun v -> cast_typ_M abs_typ_impl tret v))
  | p :: l ->
    build_funval_rec arch abs_typ_impl te ge STree.empty (p :: l) tret e

(** val build_fun_value :
    archi -> __ PMap.t -> tenv -> genv -> btyp SMapList.t -> btyp -> expr ->
    value res **)

let build_fun_value arch abs_typ_impl te ge params tret e =
  if SMapList.nodup params
  then MonError.bind (btyp_to_typ te tret) (fun tret' ->
         MonError.bind (SMapList.map_err (btyp_to_typ te) params)
           (fun params' ->
           MonError.ret (Val ((TFun ((map snd params'), tret')),
             (build_funval arch abs_typ_impl te ge params' tret' e)))))
  else MonError.fail

(** val fields_btyp_to_typ : tenv -> btyp SMapList.t -> typ SMapList.t res **)

let fields_btyp_to_typ te fields =
  SMapList.map_err (btyp_to_typ te) fields

(** val eval_def_type :
    tenv -> Syntax.ident -> (Syntax.ident * btyp) list -> tenv res **)

let eval_def_type te x fields =
  MonError.bind (fields_btyp_to_typ te fields) (fun fields' ->
    tenv_update te x fields')

(** val eval_def_const :
    __ PMap.t -> tenv -> genv -> Syntax.ident -> literal -> btyp -> genv res **)

let eval_def_const abs_typ_impl te ge x l ty =
  MonError.bind (eval_literal abs_typ_impl te l) (fun vv ->
    let Val (tv, _) = vv in
    MonError.bind (btyp_to_typ te ty) (fun ty' ->
      if typ_eq_dec tv ty'
      then genv_update abs_typ_impl ge x vv
      else MonError.fail))

(** val eval_def_fun :
    archi -> __ PMap.t -> tenv -> genv -> Syntax.ident -> coq_function ->
    genv res **)

let eval_def_fun arch abs_typ_impl te ge x f =
  MonError.bind
    (build_fun_value arch abs_typ_impl te ge f.fn_params f.fn_return
      f.fn_body) (fun fv -> genv_update abs_typ_impl ge x fv)

(** val interpret_rec :
    archi -> __ PMap.t -> tenv -> genv -> command list -> value list res **)

let rec interpret_rec arch abs_typ_impl te ge = function
| [] -> MonError.ret []
| c :: xprog' ->
  (match c with
   | CmdDef def ->
     (match def with
      | DefType (x, fields) ->
        MonError.bind (eval_def_type te x fields) (fun te' ->
          interpret_rec arch abs_typ_impl te' ge xprog')
      | DefConst (x, l, ty) ->
        MonError.bind (eval_def_const abs_typ_impl te ge x l ty) (fun ge' ->
          interpret_rec arch abs_typ_impl te ge' xprog')
      | DefFun (x, f) ->
        MonError.bind (eval_def_fun arch abs_typ_impl te ge x f) (fun ge' ->
          interpret_rec arch abs_typ_impl te ge' xprog')
      | DeclType (_, _) ->
        MonError.failwith
          ('t'::('h'::('e'::(' '::('p'::('r'::('o'::('g'::('r'::('a'::('m'::(' '::('c'::('o'::('n'::('t'::('a'::('i'::('n'::('s'::(' '::('a'::('b'::('s'::('t'::('r'::('a'::('c'::('t'::(' '::('t'::('y'::('p'::('e'::('s'::[])))))))))))))))))))))))))))))))))))
      | _ ->
        MonError.failwith
          ('t'::('h'::('e'::(' '::('p'::('r'::('o'::('g'::('r'::('a'::('m'::(' '::('c'::('o'::('n'::('t'::('a'::('i'::('n'::('s'::(' '::('a'::('b'::('s'::('t'::('r'::('a'::('c'::('t'::(' '::('d'::('e'::('f'::('i'::('n'::('i'::('t'::('i'::('o'::('n'::('s'::[]))))))))))))))))))))))))))))))))))))))))))
   | CmdExpr e ->
     MonError.bind (eval_expr arch abs_typ_impl te ge STree.empty e)
       (fun v ->
       MonError.bind (interpret_rec arch abs_typ_impl te ge xprog') (fun l ->
         MonError.ret (v :: l))))

(** val interpret : archi -> __ PMap.t -> iprogram -> value list res **)

let interpret arch abs_typ_impl iprog =
  interpret_rec arch abs_typ_impl STree.empty STree.empty iprog

(** val eval_def_rec :
    archi -> __ PMap.t -> tenv -> genv -> program -> Syntax.ident -> value res **)

let rec eval_def_rec arch abs_typ_impl te ge prog x =
  match prog with
  | [] -> MonError.fail
  | d :: prog' ->
    (match d with
     | DefType (y, fields) ->
       MonError.bind (eval_def_type te x fields) (fun te' ->
         eval_def_rec arch abs_typ_impl te' ge prog' y)
     | DefConst (y, l, ty) ->
       MonError.bind (eval_def_const abs_typ_impl te ge y l ty) (fun ge' ->
         if eq_dec x y
         then genv_get abs_typ_impl ge' x
         else eval_def_rec arch abs_typ_impl te ge' prog' x)
     | DefFun (y, f) ->
       MonError.bind (eval_def_fun arch abs_typ_impl te ge y f) (fun ge' ->
         if eq_dec x y
         then genv_get abs_typ_impl ge' x
         else eval_def_rec arch abs_typ_impl te ge' prog' x)
     | DeclType (_, _) -> eval_def_rec arch abs_typ_impl te ge prog' x
     | DeclConst (y, _) ->
       if eq_dec x y
       then genv_get abs_typ_impl ge x
       else eval_def_rec arch abs_typ_impl te ge prog' x
     | DeclFun (y, _, _) ->
       if eq_dec x y
       then genv_get abs_typ_impl ge x
       else eval_def_rec arch abs_typ_impl te ge prog' x)

(** val eval_def :
    archi -> __ PMap.t -> genv -> program -> Syntax.ident -> value res **)

let eval_def arch abs_typ_impl impl prog x =
  eval_def_rec arch abs_typ_impl STree.empty impl prog x
