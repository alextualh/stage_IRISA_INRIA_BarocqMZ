open Errors
open List0
open Maps2
open Monads
open Syntax
open Target
open Types
open Typing
open Utils

type literal = Syntax.literal

type atom = Syntax.atom

type statement =
| StSet of ident * comp
| StIfThenElse of atom * statement * statement
| StSequence of statement * statement
| StReturn of atom

type coq_function = statement Syntax.coq_function

type globdef = (literal, coq_function) Syntax.globdef

type program = globdef Syntax.program

module Typed =
 struct
  type literal = Typed.literal

  type atom = Typed.atom

  type comp = Typed.comp

  type statement =
  | StSet of ident * comp
  | StIfThenElse of atom * statement * statement
  | StSequence of statement * statement
  | StReturn of atom

  (** val statement_rect :
      (ident -> comp -> 'a1) -> (atom -> statement -> 'a1 -> statement -> 'a1
      -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> (atom ->
      'a1) -> statement -> 'a1 **)

  let rec statement_rect f f0 f1 f2 = function
  | StSet (i, c) -> f i c
  | StIfThenElse (a, s0, s1) ->
    f0 a s0 (statement_rect f f0 f1 f2 s0) s1 (statement_rect f f0 f1 f2 s1)
  | StSequence (s0, s1) ->
    f1 s0 (statement_rect f f0 f1 f2 s0) s1 (statement_rect f f0 f1 f2 s1)
  | StReturn a -> f2 a

  (** val statement_rec :
      (ident -> comp -> 'a1) -> (atom -> statement -> 'a1 -> statement -> 'a1
      -> 'a1) -> (statement -> 'a1 -> statement -> 'a1 -> 'a1) -> (atom ->
      'a1) -> statement -> 'a1 **)

  let rec statement_rec f f0 f1 f2 = function
  | StSet (i, c) -> f i c
  | StIfThenElse (a, s0, s1) ->
    f0 a s0 (statement_rec f f0 f1 f2 s0) s1 (statement_rec f f0 f1 f2 s1)
  | StSequence (s0, s1) ->
    f1 s0 (statement_rec f f0 f1 f2 s0) s1 (statement_rec f f0 f1 f2 s1)
  | StReturn a -> f2 a

  type coq_function = statement Syntax.coq_function

  type globdef = (literal, coq_function) Syntax.globdef

  type program = globdef Syntax.program
 end

module Aliasing_AST =
 struct
  type coq_ABSDOM = Aliasing_defs.AbsDom.t

  type literal = Syntax.Typed.literal

  type atom = Syntax.Typed.atom

  type comp = Syntax.Typed.comp

  type statement =
  | StSet of ident * comp * coq_ABSDOM * coq_ABSDOM
  | StIfThenElse of atom * statement * statement * coq_ABSDOM * coq_ABSDOM
  | StSequence of statement * statement
  | StReturn of atom * coq_ABSDOM * coq_ABSDOM

  type coq_function = statement Syntax.coq_function

  type globdef = (literal, coq_function) Syntax.globdef

  type program = globdef Syntax.program
 end

module Imp1Typed = Typed

module Typing =
 struct
  (** val typof_var : gcontext -> lcontext -> ident -> btyp res **)

  let typof_var gx lx x =
    match lcontext_get lx x with
    | OK ty -> MonError.ret ty
    | Error _ ->
      (match gcontext_get gx x with
       | OK ty ->
         (match ty with
          | BArray _ ->
            MonError.failwith
              ('I'::('m'::('p'::('1'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('o'::('f'::('_'::('v'::('a'::('r'::(':'::(' '::('t'::('h'::('e'::(' '::('u'::('s'::('e'::(' '::('o'::('f'::(' '::('n'::('o'::('n'::('-'::('p'::('r'::('i'::('m'::('i'::('t'::('i'::('v'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::('s'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | BRecord _ ->
            MonError.failwith
              ('I'::('m'::('p'::('1'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('o'::('f'::('_'::('v'::('a'::('r'::(':'::(' '::('t'::('h'::('e'::(' '::('u'::('s'::('e'::(' '::('o'::('f'::(' '::('n'::('o'::('n'::('-'::('p'::('r'::('i'::('m'::('i'::('t'::('i'::('v'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::('s'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | BAbs _ ->
            MonError.failwith
              ('I'::('m'::('p'::('1'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('o'::('f'::('_'::('v'::('a'::('r'::(':'::(' '::('t'::('h'::('e'::(' '::('u'::('s'::('e'::(' '::('o'::('f'::(' '::('n'::('o'::('n'::('-'::('p'::('r'::('i'::('m'::('i'::('t'::('i'::('v'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::('s'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ -> MonError.ret ty)
       | Error e -> Error e)

  (** val typecheck_atom :
      gcontext -> lcontext -> Syntax.atom -> Syntax.Typed.atom res **)

  let rec typecheck_atom gx lx = function
  | ATrue -> MonError.ret (Syntax.Typed.ATrue BBool)
  | AFalse -> MonError.ret (Syntax.Typed.AFalse BBool)
  | AInt32 (i, s) -> MonError.ret (Syntax.Typed.AInt32 (i, (BInt32 s)))
  | AInt64 (i, s) -> MonError.ret (Syntax.Typed.AInt64 (i, (BInt64 s)))
  | AVar x ->
    MonError.bind (typof_var gx lx x) (fun t ->
      MonError.ret (Syntax.Typed.AVar (x, t)))
  | ACast (a1, ty) ->
    MonError.bind (typecheck_atom gx lx a1) (fun a1' ->
      MonError.bind (typecheck_cast (typof_atom a1') ty) (fun t ->
        MonError.ret (Syntax.Typed.ACast (a1', t))))
  | AUnaryOp (op, a1) ->
    MonError.bind (typecheck_atom gx lx a1) (fun a1' ->
      let ty1 = typof_atom a1' in
      MonError.bind (typecheck_unary_op op ty1) (fun t ->
        MonError.ret (Syntax.Typed.AUnaryOp (op, a1', t))))
  | ABinaryOp (op, a1, a2) ->
    MonError.bind (typecheck_atom gx lx a1) (fun a1' ->
      MonError.bind (typecheck_atom gx lx a2) (fun a2' ->
        let ty1 = typof_atom a1' in
        let ty2 = typof_atom a2' in
        MonError.bind (typecheck_binary_op op ty1 ty2) (fun t ->
          MonError.ret (Syntax.Typed.ABinaryOp (op, a1', a2', t)))))

  (** val typecheck_access :
      archi -> renv -> gcontext -> lcontext -> btyp -> access list ->
      (btyp * Syntax.Typed.access list) res **)

  let rec typecheck_access arch re gx lx ty = function
  | [] -> MonError.ret (ty, [])
  | ac :: acs' ->
    (match ac with
     | AcRecordField f ->
       MonError.bind (typecheck_record_proj re ty f) (fun ty' ->
         MonError.bind2 (typecheck_access arch re gx lx ty' acs')
           (fun r lr ->
           MonError.ret (r, ((Syntax.Typed.AcRecordField (f, ty')) :: lr))))
     | AcArrayIndex ai ->
       MonError.bind (typecheck_atom gx lx ai) (fun ai' ->
         MonError.bind (typecheck_array_get arch ty (typof_atom ai'))
           (fun ty' ->
           MonError.bind2 (typecheck_access arch re gx lx ty' acs')
             (fun r lr ->
             MonError.ret (r, ((Syntax.Typed.AcArrayIndex (ai', ty')) :: lr))))))

  (** val typecheck_comp :
      archi -> renv -> gcontext -> lcontext -> comp -> Imp1Typed.comp res **)

  let typecheck_comp arch re gx lx = function
  | CpAtom a ->
    MonError.bind (typecheck_atom gx lx a) (fun a' ->
      MonError.ret (Syntax.Typed.CpAtom (a', (typof_atom a'))))
  | CpArrayGet (a1, a2) ->
    MonError.bind (typecheck_atom gx lx a1) (fun a1' ->
      MonError.bind (typecheck_atom gx lx a2) (fun a2' ->
        let ty1 = typof_atom a1' in
        let ty2 = typof_atom a2' in
        MonError.bind (typecheck_array_get arch ty1 ty2) (fun ty ->
          MonError.ret (Syntax.Typed.CpArrayGet (a1', a2', ty)))))
  | CpArraySet (a1, a2, a3) ->
    MonError.bind (typecheck_atom gx lx a1) (fun a1' ->
      MonError.bind (typecheck_atom gx lx a2) (fun a2' ->
        MonError.bind (typecheck_atom gx lx a3) (fun a3' ->
          let ty1 = typof_atom a1' in
          let ty2 = typof_atom a2' in
          let ty3 = typof_atom a3' in
          MonError.bind (typecheck_array_set arch ty1 ty2 ty3) (fun ty ->
            MonError.ret (Syntax.Typed.CpArraySet (a1', a2', a3', ty))))))
  | CpRecordProj (a, x) ->
    MonError.bind (typecheck_atom gx lx a) (fun a' ->
      let tya = typof_atom a' in
      MonError.bind (typecheck_record_proj re tya x) (fun ty ->
        MonError.ret (Syntax.Typed.CpRecordProj (a', x, ty))))
  | CpRecordUpdate (a1, x, a2) ->
    MonError.bind (typecheck_atom gx lx a1) (fun a1' ->
      MonError.bind (typecheck_atom gx lx a2) (fun a2' ->
        let ty1 = typof_atom a1' in
        let ty2 = typof_atom a2' in
        MonError.bind (typecheck_record_update re ty1 ty2 x) (fun ty ->
          MonError.ret (Syntax.Typed.CpRecordUpdate (a1', x, a2', ty)))))
  | CpDeepAccess (a, acs) ->
    MonError.bind (typecheck_atom gx lx a) (fun a' ->
      MonError.bind2 (typecheck_access arch re gx lx (typof_atom a') acs)
        (fun t acs' -> MonError.ret (Syntax.Typed.CpDeepAccess (a', acs', t))))
  | CpCall (a, args) ->
    MonError.bind (typecheck_atom gx lx a) (fun a' ->
      let tya = typof_atom a' in
      MonError.bind (mmap (typecheck_atom gx lx) args) (fun args' ->
        let targs = map typof_atom args' in
        MonError.bind (typecheck_call tya targs) (fun ty ->
          MonError.ret (Syntax.Typed.CpCall (a', args', ty)))))

  (** val merge_context : lcontext -> lcontext -> lcontext res **)

  let merge_context lx1 lx2 =
    STree.fold (fun acc k v ->
      MonError.bind acc (fun acc0 -> lcontext_update acc0 k v)) lx2
      (MonError.ret lx1)

  (** val typecheck_statement :
      archi -> renv -> gcontext -> lcontext -> btyp -> statement ->
      (Imp1Typed.statement * lcontext) res **)

  let rec typecheck_statement arch re gx lx tret = function
  | StSet (x, c) ->
    MonError.bind (typecheck_comp arch re gx lx c) (fun c' ->
      MonError.bind (lcontext_update lx x (typof_comp c')) (fun lx' ->
        MonError.ret ((Imp1Typed.StSet (x, c')), lx')))
  | StIfThenElse (a, s1, s2) ->
    MonError.bind2 (typecheck_statement arch re gx lx tret s1)
      (fun s1' lx1 ->
      MonError.bind2 (typecheck_statement arch re gx lx tret s2)
        (fun s2' lx2 ->
        MonError.bind (typecheck_atom gx lx a) (fun a' ->
          match typof_atom a' with
          | BBool ->
            MonError.bind (merge_context lx1 lx2) (fun lx' ->
              MonError.ret ((Imp1Typed.StIfThenElse (a', s1', s2')), lx'))
          | _ ->
            MonError.failwith
              ('I'::('m'::('p'::('1'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('s'::('t'::('a'::('t'::('e'::('m'::('e'::('n'::('t'::(':'::(' '::('a'::('t'::('o'::('m'::(' '::('o'::('f'::(' '::('t'::('y'::('p'::('e'::(' '::('b'::('o'::('o'::('l'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | StSequence (s1, s2) ->
    MonError.bind2 (typecheck_statement arch re gx lx tret s1)
      (fun s1' lx1 ->
      MonError.bind2 (typecheck_statement arch re gx lx1 tret s2)
        (fun s2' lx2 -> MonError.ret ((Imp1Typed.StSequence (s1', s2')), lx2)))
  | StReturn a ->
    MonError.bind (typecheck_atom gx lx a) (fun a' ->
      let ty = typof_atom a' in
      if btyp_eq_dec ty tret
      then MonError.ret ((Imp1Typed.StReturn a'), lx)
      else MonError.failwith
             ('I'::('m'::('p'::('1'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('s'::('t'::('a'::('t'::('e'::('m'::('e'::('n'::('t'::(':'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))

  (** val typecheck_function :
      archi -> renv -> gcontext -> coq_function -> Imp1Typed.coq_function res **)

  let typecheck_function arch re gx f =
    MonError.bind
      (fold_left_err (fun acc pat ->
        let (x, tx) = pat in lcontext_update acc x tx) f.fn_params
        (MonError.ret STree.empty)) (fun lx ->
      MonError.bind2
        (typecheck_statement arch re gx lx f.fn_return f.fn_body)
        (fun body _ ->
        MonError.ret { fn_return = f.fn_return; fn_params = f.fn_params;
          fn_body = body }))

  (** val typecheck_globdefs_rec :
      archi -> renv -> gcontext -> globdef list -> Imp1Typed.globdef list res **)

  let rec typecheck_globdefs_rec arch re gx = function
  | [] -> MonError.ret []
  | d :: defs' ->
    (match d with
     | DefConst (x, l, ty) ->
       MonError.bind (typecheck_literal re l) (fun l' ->
         if btyp_eq_dec ty (typof_literal l')
         then MonError.bind (gcontext_update gx x ty) (fun gx0 ->
                MonError.bind (typecheck_globdefs_rec arch re gx0 defs')
                  (fun rd -> MonError.ret ((DefConst (x, l', ty)) :: rd)))
         else MonError.failwith
                ('I'::('m'::('p'::('1'::('.'::('T'::('y'::('p'::('i'::('n'::('g'::('.'::('t'::('y'::('p'::('e'::('c'::('h'::('e'::('c'::('k'::('_'::('g'::('l'::('o'::('b'::('d'::('e'::('f'::('s'::(':'::(' '::('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::(' '::('i'::('n'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(' '::('d'::('e'::('f'::('i'::('n'::('i'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | DefFun (x, f) ->
       MonError.bind (typecheck_function arch re gx f) (fun f' ->
         MonError.bind
           (gcontext_update gx x (mk_fun_btyp f'.fn_params f'.fn_return))
           (fun gx0 ->
           MonError.bind (typecheck_globdefs_rec arch re gx0 defs')
             (fun rd -> MonError.ret ((DefFun (x, f')) :: rd))))
     | DeclConst (x, ty) ->
       MonError.bind (gcontext_update gx x ty) (fun gx0 ->
         MonError.bind (typecheck_globdefs_rec arch re gx0 defs') (fun rd ->
           MonError.ret ((DeclConst (x, ty)) :: rd)))
     | DeclFun (x, tparams, tret) ->
       MonError.bind (gcontext_update gx x (mk_fun_btyp tparams tret))
         (fun gx0 ->
         MonError.bind (typecheck_globdefs_rec arch re gx0 defs') (fun rd ->
           MonError.ret ((DeclFun (x, tparams, tret)) :: rd))))

  (** val typecheck_globdefs :
      archi -> renv -> globdef list -> Imp1Typed.globdef list res **)

  let typecheck_globdefs arch re defs =
    typecheck_globdefs_rec arch re STree.empty defs

  (** val typecheck_program : archi -> program -> Imp1Typed.program res **)

  let typecheck_program arch prog =
    MonError.bind
      (fold_left_err (fun acc td ->
        match td with
        | TdRecord rd -> renv_update acc rd.rd_name rd.rd_fields
        | TdAbstract (_, _) -> MonError.ret acc) prog.prog_types
        (MonError.ret STree.empty)) (fun re ->
      MonError.bind (typecheck_globdefs arch re prog.prog_defs) (fun defs ->
        MonError.ret { prog_defs = defs; prog_types = prog.prog_types }))
 end

module Imp1Typing = Typing
