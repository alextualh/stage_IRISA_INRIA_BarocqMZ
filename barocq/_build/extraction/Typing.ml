open Barray
open Errors
open Maps2
open Monads
open Syntax
open Target
open Types

type renv = btyp SMapList.t STree.t

(** val renv_get : renv -> Syntax.ident -> btyp SMapList.t res **)

let renv_get re x =
  MonError.err_of_opt (STree.get x re)

(** val renv_update : renv -> Syntax.ident -> btyp SMapList.t -> renv res **)

let renv_update re x fields =
  match renv_get re x with
  | OK _ -> MonError.fail
  | Error _ -> MonError.ret (STree.set x fields re)

type tenv = typ SMapList.t STree.t

(** val tenv_get : tenv -> Syntax.ident -> typ SMapList.t res **)

let tenv_get te x =
  MonError.err_of_opt (STree.get x te)

(** val tenv_update : tenv -> Syntax.ident -> typ SMapList.t -> tenv res **)

let tenv_update te x fields =
  match tenv_get te x with
  | OK _ -> MonError.fail
  | Error _ -> MonError.ret (STree.set x fields te)

(** val btyp_to_typ : tenv -> btyp -> typ res **)

let rec btyp_to_typ te = function
| BBool -> MonError.ret TBool
| BInt32 s -> MonError.ret (TInt32 s)
| BInt64 s -> MonError.ret (TInt64 s)
| BArray ta ->
  MonError.bind (btyp_to_typ te ta) (fun ta' -> MonError.ret (TArray ta'))
| BRecord tx ->
  MonError.bind (tenv_get te tx) (fun fields ->
    MonError.ret (TRecord (tx, fields)))
| BFun (tparams, tret) ->
  MonError.bind (mmap (btyp_to_typ te) tparams) (fun tparams' ->
    MonError.bind (btyp_to_typ te tret) (fun tret' ->
      MonError.ret (TFun (tparams', tret'))))
| BAbs t0 -> MonError.ret (TAbs t0)

(** val typof_literal : Typed.literal -> btyp **)

let typof_literal = function
| Typed.LTrue ty -> ty
| Typed.LFalse ty -> ty
| Typed.LInt32 (_, ty) -> ty
| Typed.LInt64 (_, ty) -> ty
| Typed.LArray (_, ty) -> ty
| Typed.LRecord (_, ty) -> ty

(** val typof_atom : Typed.atom -> btyp **)

let typof_atom = function
| Typed.ATrue ty -> ty
| Typed.AFalse ty -> ty
| Typed.AInt32 (_, ty) -> ty
| Typed.AInt64 (_, ty) -> ty
| Typed.AVar (_, ty) -> ty
| Typed.ACast (_, ty) -> ty
| Typed.AUnaryOp (_, _, ty) -> ty
| Typed.ABinaryOp (_, _, _, ty) -> ty

(** val typof_comp : Typed.comp -> btyp **)

let typof_comp = function
| Typed.CpAtom (_, ty) -> ty
| Typed.CpArrayGet (_, _, ty) -> ty
| Typed.CpArraySet (_, _, _, ty) -> ty
| Typed.CpRecordProj (_, _, ty) -> ty
| Typed.CpRecordUpdate (_, _, _, ty) -> ty
| Typed.CpDeepAccess (_, _, ty) -> ty
| Typed.CpCall (_, _, ty) -> ty

type gcontext = btyp STree.t

type lcontext = btyp STree.t

(** val gcontext_get : gcontext -> Syntax.ident -> btyp res **)

let gcontext_get gx x =
  match STree.get x gx with
  | Some t0 -> MonError.ret t0
  | None ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('g'::('c'::('o'::('n'::('t'::('e'::('x'::('t'::('_'::('g'::('e'::('t'::(':'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('i'::('d'::('e'::('n'::('t'::('i'::('f'::('i'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))))

(** val gcontext_update : gcontext -> Syntax.ident -> btyp -> gcontext res **)

let gcontext_update gx x ty =
  match gcontext_get gx x with
  | OK _ ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('g'::('c'::('o'::('n'::('t'::('e'::('x'::('t'::('_'::('u'::('p'::('d'::('a'::('t'::('e'::(':'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::(' '::('a'::('l'::('r'::('e'::('a'::('d'::('y'::(' '::('d'::('e'::('f'::('i'::('n'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Error _ -> MonError.ret (STree.set x ty gx)

(** val lcontext_get : lcontext -> Syntax.ident -> btyp res **)

let lcontext_get lx x =
  match STree.get x lx with
  | Some t0 -> MonError.ret t0
  | None ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('l'::('c'::('o'::('n'::('t'::('e'::('x'::('t'::('_'::('g'::('e'::('t'::(':'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('i'::('d'::('e'::('n'::('t'::('i'::('f'::('i'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))))

(** val lcontext_update : lcontext -> Syntax.ident -> btyp -> lcontext res **)

let lcontext_update lx x ty =
  match lcontext_get lx x with
  | OK t0 ->
    if btyp_eq_dec ty t0
    then MonError.ret (STree.set x ty lx)
    else MonError.failwith
           ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('l'::('c'::('o'::('n'::('t'::('e'::('x'::('t'::('_'::('u'::('p'::('d'::('a'::('t'::('e'::(':'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('s'::('h'::('a'::('d'::('o'::('w'::('i'::('n'::('g'::(' '::('w'::('i'::('t'::('h'::(' '::('a'::(' '::('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::(' '::('t'::('y'::('p'::('e'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Error _ -> MonError.ret (STree.set x ty lx)

(** val typof_var : gcontext -> lcontext -> Syntax.ident -> btyp res **)

let typof_var gx lx x =
  match lcontext_get lx x with
  | OK ty -> MonError.ret ty
  | Error _ ->
    MonError.bind_catch (gcontext_get gx x) MonError.ret
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('o'::('f'::('_'::('v'::('a'::('r'::(':'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::[]))))))))))))))))))))))))))))))))))

(** val typecheck_cast : btyp -> btyp -> btyp res **)

let typecheck_cast from to0 =
  match from with
  | BBool ->
    (match to0 with
     | BBool -> MonError.ret to0
     | BInt32 _ -> MonError.ret to0
     | BInt64 _ -> MonError.ret to0
     | _ -> MonError.fail)
  | BInt32 _ ->
    (match to0 with
     | BBool -> MonError.ret to0
     | BInt32 _ -> MonError.ret to0
     | BInt64 _ -> MonError.ret to0
     | _ -> MonError.fail)
  | BInt64 _ ->
    (match to0 with
     | BBool -> MonError.ret to0
     | BInt32 _ -> MonError.ret to0
     | BInt64 _ -> MonError.ret to0
     | _ -> MonError.fail)
  | _ -> MonError.fail

(** val typecheck_unary_op : unary_op -> btyp -> btyp res **)

let typecheck_unary_op op ty =
  match op with
  | UopNotbool ->
    (match ty with
     | BBool -> MonError.ret ty
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('u'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))
  | _ ->
    (match ty with
     | BInt32 _ -> MonError.ret ty
     | BInt64 _ -> MonError.ret ty
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('u'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))

(** val typecheck_binary_op : binary_op -> btyp -> btyp -> btyp res **)

let typecheck_binary_op op ty1 ty2 =
  match op with
  | BopAndbool ->
    (match ty1 with
     | BBool ->
       (match ty2 with
        | BBool -> MonError.ret ty1
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopOrbool ->
    (match ty1 with
     | BBool ->
       (match ty2 with
        | BBool -> MonError.ret ty1
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopXorbool ->
    (match ty1 with
     | BBool ->
       (match ty2 with
        | BBool -> MonError.ret ty1
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopEq ->
    (match ty1 with
     | BBool ->
       (match ty2 with
        | BBool -> MonError.ret ty1
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopNeq ->
    (match ty1 with
     | BBool ->
       (match ty2 with
        | BBool -> MonError.ret ty1
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopLt ->
    (match ty1 with
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopGt ->
    (match ty1 with
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopLe ->
    (match ty1 with
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | BopGe ->
    (match ty1 with
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret BBool
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    (match ty1 with
     | BInt32 s1 ->
       (match ty2 with
        | BInt32 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret ty1
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | BInt64 s1 ->
       (match ty2 with
        | BInt64 s2 ->
          if signedness_eq_dec s1 s2
          then MonError.ret ty1
          else MonError.failwith
                 ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::(' '::('s'::('i'::('g'::('n'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          MonError.failwith
            ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))
     | _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_array_get : archi -> btyp -> btyp -> btyp res **)

let typecheck_array_get arch ty1 ty2 =
  match ty1 with
  | BArray ta ->
    if btyp_eq_dec ty2 (arr_index_btyp arch)
    then MonError.ret ta
    else MonError.failwith
           ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('g'::('e'::('t'::(':'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('g'::('e'::('t'::(':'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('t'::('y'::('p'::('e'::('d'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_array_set : archi -> btyp -> btyp -> btyp -> btyp res **)

let typecheck_array_set arch ty1 ty2 ty3 =
  match ty1 with
  | BArray ta ->
    if btyp_eq_dec ty2 (arr_index_btyp arch)
    then if btyp_eq_dec ta ty3
         then MonError.ret ty1
         else MonError.failwith
                ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('s'::('e'::('t'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))
    else MonError.failwith
           ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('s'::('e'::('t'::(':'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('s'::('e'::('t'::(':'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('t'::('y'::('p'::('e'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_record_proj : renv -> btyp -> Syntax.ident -> btyp res **)

let typecheck_record_proj re ty x =
  match ty with
  | BRecord t0 ->
    MonError.bind_catch (renv_get re t0) (fun fields ->
      btypof_field x fields)
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('r'::('e'::('c'::('o'::('r'::('d'::('_'::('p'::('r'::('o'::('j'::(':'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::('t'::('y'::('p'::('e'::[])))))))))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('r'::('e'::('c'::('o'::('r'::('d'::('_'::('p'::('r'::('o'::('j'::(':'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_record_update :
    renv -> btyp -> btyp -> Syntax.ident -> btyp res **)

let typecheck_record_update re ty1 ty2 x =
  match ty1 with
  | BRecord t0 ->
    MonError.bind_catch (renv_get re t0) (fun fields ->
      MonError.bind (btypof_field x fields) (fun tx ->
        if btyp_eq_dec tx ty2
        then MonError.ret ty1
        else MonError.failwith
               ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('r'::('e'::('c'::('o'::('r'::('d'::('_'::('u'::('p'::('d'::('a'::('t'::('e'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))))))
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('r'::('e'::('c'::('o'::('r'::('d'::('_'::('p'::('r'::('o'::('j'::(':'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::('t'::('y'::('p'::('e'::[])))))))))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('r'::('e'::('c'::('o'::('r'::('d'::('_'::('u'::('p'::('d'::('a'::('t'::('e'::(':'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_call_rec : btyp list -> btyp list -> btyp -> btyp res **)

let rec typecheck_call_rec tparams targs tret =
  match tparams with
  | [] ->
    (match targs with
     | [] -> MonError.ret tret
     | _ :: _ ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('c'::('a'::('l'::('l'::('_'::('r'::('e'::('c'::(':'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('n'::('u'::('m'::('b'::('e'::('r'::(' '::('o'::('f'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))
  | tp1 :: tparams' ->
    (match targs with
     | [] ->
       MonError.failwith
         ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('c'::('a'::('l'::('l'::('_'::('r'::('e'::('c'::(':'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('n'::('u'::('m'::('b'::('e'::('r'::(' '::('o'::('f'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))
     | ta1 :: targs' ->
       if btyp_eq_dec tp1 ta1
       then typecheck_call_rec tparams' targs' tret
       else MonError.failwith
              ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('c'::('a'::('l'::('l'::('_'::('r'::('e'::('c'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))

(** val typecheck_call : btyp -> btyp list -> btyp res **)

let typecheck_call ty targs =
  match ty with
  | BFun (tparams, tret) -> typecheck_call_rec tparams targs tret
  | _ ->
    MonError.failwith
      ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('c'::('a'::('l'::('l'::(':'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_array_lit : Typed.literal array -> btyp res **)

let rec typecheck_array_lit = function
| [] ->
  MonError.failwith
    ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('l'::('i'::('t'::(':'::(' '::('e'::('m'::('p'::('t'::('y'::(' '::('a'::('r'::('r'::('a'::('y'::[])))))))))))))))))))))))))))))))))))))))
| l :: a' ->
  (match a' with
   | [] -> MonError.ret (typof_literal l)
   | _ :: _ ->
     MonError.bind (typecheck_array_lit a') (fun t0 ->
       if btyp_eq_dec (typof_literal l) t0
       then MonError.ret t0
       else MonError.failwith
              ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('l'::('i'::('t'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))))))))))))))))))))))

(** val typecheck_struct_lit :
    Typed.literal SMapList.t -> btyp SMapList.t -> bool **)

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
       if btyp_eq_dec tx1 tx2 then typecheck_struct_lit l1' l2' else false)

(** val typecheck_literal : renv -> literal -> Typed.literal res **)

let rec typecheck_literal re = function
| LTrue -> MonError.ret (Typed.LTrue BBool)
| LFalse -> MonError.ret (Typed.LFalse BBool)
| LInt32 (i, s) -> MonError.ret (Typed.LInt32 (i, (BInt32 s)))
| LInt64 (i, s) -> MonError.ret (Typed.LInt64 (i, (BInt64 s)))
| LArray a ->
  MonError.bind (mmap (typecheck_literal re) a) (fun a' ->
    MonError.bind (typecheck_array_lit a') (fun t0 ->
      MonError.ret (Typed.LArray (a', (BArray t0)))))
| LRecord (rc, x) ->
  MonError.bind (SMapList.map_err (typecheck_literal re) rc) (fun rc' ->
    MonError.bind (renv_get re x) (fun t0 ->
      if typecheck_struct_lit rc' t0
      then MonError.ret (Typed.LRecord (rc', (BRecord x)))
      else MonError.failwith
             ('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('t'::('e'::('r'::('a'::('l'::(':'::(' '::('r'::('e'::('c'::('o'::('r'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))))))))
