open AST
open Barray
open BinInt
open BinNums
open Clight
open ClightCe
open Cop
open Ctypes
open Ctypesdefs
open Datatypes
open Errors
open Ident
open Imp2
open Integers
open List0
open MSetPositive
open Maps
open Maps2
open Monads
open Syntax
open Types
open Utils

(** val transl_btyp : coq_type PMap.t -> btyp -> coq_type **)

let rec transl_btyp abs_types_impl = function
| BBool -> tbool
| BInt32 s -> (match s with
               | Signed -> tint
               | Unsigned -> tuint)
| BInt64 s -> (match s with
               | Signed -> tlong
               | Unsigned -> tulong)
| BArray ta -> tptr (transl_btyp abs_types_impl ta)
| BRecord t0 -> tptr (Tstruct ((to_pos t0), noattr))
| BFun (tparams, tret) ->
  let tparams' = map (transl_btyp abs_types_impl) tparams in
  let tret' = transl_btyp abs_types_impl tret in
  tptr (Tfunction (tparams', tret', cc_default))
| BAbs t0 -> tptr (PMap.get (to_pos t0) abs_types_impl)

(** val transl_btyp_to_xtype : btyp -> xtype **)

let transl_btyp_to_xtype = function
| BBool -> Xbool
| BInt32 _ -> Xint
| BInt64 _ -> Xlong
| _ -> Xptr

(** val transl_btyp_lit : coq_type PMap.t -> btyp -> coq_Z -> coq_type **)

let transl_btyp_lit abs_types_impl ty n =
  match ty with
  | BArray ta -> Tarray ((transl_btyp abs_types_impl ta), n, noattr)
  | BRecord t0 -> Tstruct ((to_pos t0), noattr)
  | _ -> transl_btyp abs_types_impl ty

(** val transl_literal_base : literal_base -> init_data **)

let transl_literal_base = function
| LbTrue -> Init_int32 Int.one
| LbFalse -> Init_int32 Int.zero
| LbInt32 i -> Init_int32 i
| LbInt64 i -> Init_int64 i
| LbVar x -> Init_addrof ((to_pos x), Ptrofs.zero)

(** val transl_literal : Imp2.literal -> init_data list **)

let transl_literal = function
| LBase (b, _) -> (transl_literal_base b) :: []
| Imp2.LArray (a, _) -> map transl_literal_base a
| Imp2.LRecord (st, _) ->
  map (fun pat -> let (_, lx) = pat in transl_literal_base lx) st

(** val transl_unary_op : unary_op -> unary_operation res **)

let transl_unary_op = function
| UopNotbool -> MonError.ret Onotbool
| UopNotint -> MonError.ret Onotint
| UopNeg -> MonError.ret Oneg
| UopPlus -> MonError.fail

(** val transl_binary_op : binary_op -> binary_operation res **)

let transl_binary_op = function
| BopAndbool ->
  MonError.failwith
    ('C'::('l'::('i'::('g'::('h'::('t'::('C'::('e'::('g'::('e'::('n'::('.'::('t'::('r'::('a'::('n'::('s'::('l'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('u'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('b'::('o'::('o'::('l'::('e'::('a'::('n'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| BopOrbool ->
  MonError.failwith
    ('C'::('l'::('i'::('g'::('h'::('t'::('C'::('e'::('g'::('e'::('n'::('.'::('t'::('r'::('a'::('n'::('s'::('l'::('_'::('b'::('i'::('n'::('a'::('r'::('y'::('_'::('o'::('p'::(':'::(' '::('u'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('b'::('o'::('o'::('l'::('e'::('a'::('n'::(' '::('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| BopAdd -> MonError.ret Oadd
| BopSub -> MonError.ret Osub
| BopMul -> MonError.ret Omul
| BopDiv -> MonError.ret Odiv
| BopMod -> MonError.ret Omod
| BopAndint -> MonError.ret Oand
| BopOrint -> MonError.ret Oor
| BopXorint -> MonError.ret Oxor
| BopShl -> MonError.ret Oshl
| BopShr -> MonError.ret Oshr
| BopEq -> MonError.ret Oeq
| BopLt -> MonError.ret Olt
| BopGt -> MonError.ret Ogt
| BopLe -> MonError.ret Ole
| BopGe -> MonError.ret Oge
| _ -> MonError.ret One

(** val transl_atom :
    coq_type PMap.t -> PositiveSet.t -> Typed.atom -> ClightCe.expr res **)

let rec transl_atom abs_types_impl globs = function
| Typed.ATrue _ -> MonError.ret (Econst_int (Int.one, tbool))
| Typed.AFalse _ -> MonError.ret (Econst_int (Int.zero, tbool))
| Typed.AInt32 (i, _) -> MonError.ret (Econst_int (i, tint))
| Typed.AInt64 (i, _) -> MonError.ret (Econst_long (i, tlong))
| Typed.AVar (x, ty) ->
  if smem globs (to_pos x)
  then MonError.ret (Evar ((to_pos x), (transl_btyp abs_types_impl ty)))
  else MonError.ret (Etempvar ((to_pos x), (transl_btyp abs_types_impl ty)))
| Typed.ACast (a0, ty) ->
  MonError.bind (transl_atom abs_types_impl globs a0) (fun e ->
    MonError.ret (Ecast (e, (transl_btyp abs_types_impl ty))))
| Typed.AUnaryOp (op, a1, ty) ->
  MonError.bind (transl_atom abs_types_impl globs a1) (fun e ->
    let t0 = transl_btyp abs_types_impl ty in
    (match transl_unary_op op with
     | OK op' -> MonError.ret (Eunop (op', e, t0))
     | Error _ -> MonError.ret e))
| Typed.ABinaryOp (op, a1, a2, ty) ->
  MonError.bind (transl_atom abs_types_impl globs a1) (fun e1 ->
    MonError.bind (transl_atom abs_types_impl globs a2) (fun e2 ->
      let t0 = transl_btyp abs_types_impl ty in
      MonError.bind (transl_binary_op op) (fun op' ->
        MonError.ret (Ebinop (op', e1, e2, t0)))))

(** val deref_pointer : coq_type -> coq_type **)

let deref_pointer ty = match ty with
| Tpointer (t0, _) -> t0
| _ -> ty

(** val transl_deep_access :
    coq_type PMap.t -> PositiveSet.t -> Imp2.atom -> Imp2.access list ->
    ClightCe.expr res **)

let rec transl_deep_access abs_types_impl globs a = function
| [] -> transl_atom abs_types_impl globs a
| ac :: acs' ->
  (match acs' with
   | [] ->
     (match ac with
      | Typed.AcRecordField (f, ty) ->
        MonError.bind (transl_atom abs_types_impl globs a) (fun e ->
          let tderef = deref_pointer (typeof e) in
          let tfield = transl_btyp abs_types_impl ty in
          MonError.ret (Efield ((Ederef (e, tderef)), (to_pos f), tfield)))
      | Typed.AcArrayIndex (ai, ty) ->
        MonError.bind (transl_atom abs_types_impl globs a) (fun e ->
          MonError.bind (transl_atom abs_types_impl globs ai) (fun ei ->
            let tarith = typeof e in
            let tderef = transl_btyp abs_types_impl ty in
            MonError.ret (Ederef ((Ebinop (Oadd, e, ei, tarith)), tderef)))))
   | _ :: _ ->
     (match ac with
      | Typed.AcRecordField (f, ty) ->
        MonError.bind (transl_deep_access abs_types_impl globs a acs')
          (fun er ->
          let tderef = deref_pointer (typeof er) in
          let tfield = transl_btyp abs_types_impl ty in
          MonError.ret (Efield ((Ederef (er, tderef)), (to_pos f), tfield)))
      | Typed.AcArrayIndex (ai, ty) ->
        MonError.bind (transl_deep_access abs_types_impl globs a acs')
          (fun er ->
          MonError.bind (transl_atom abs_types_impl globs ai) (fun ei ->
            let tarith = typeof er in
            let tderef = transl_btyp abs_types_impl ty in
            MonError.ret (Ederef ((Ebinop (Oadd, er, ei, tarith)), tderef))))))

(** val transl_expr :
    coq_type PMap.t -> PositiveSet.t -> expr -> ClightCe.expr res **)

let transl_expr abs_types_impl globs = function
| EAtom (a, _) -> transl_atom abs_types_impl globs a
| EArrayGet (a1, a2, ty) ->
  MonError.bind (transl_atom abs_types_impl globs a1) (fun e1 ->
    MonError.bind (transl_atom abs_types_impl globs a2) (fun e2 ->
      let tarith = typeof e1 in
      let tderef = transl_btyp abs_types_impl ty in
      MonError.ret (Ederef ((Ebinop (Oadd, e1, e2, tarith)), tderef))))
| ERecordProj (a, f, ty) ->
  MonError.bind (transl_atom abs_types_impl globs a) (fun e0 ->
    let tderef = deref_pointer (typeof e0) in
    let tfield = transl_btyp abs_types_impl ty in
    MonError.ret (Efield ((Ederef (e0, tderef)), (to_pos f), tfield)))
| EDeepAccess (a, acs, _) ->
  transl_deep_access abs_types_impl globs a (rev' acs)

(** val transl_ecomp :
    coq_type PMap.t -> PositiveSet.t -> ecomp ->
    (ClightCe.expr * ClightCe.statement) res **)

let transl_ecomp abs_types_impl globs = function
| EcArraySet (a1, a2, a3) ->
  MonError.bind (transl_atom abs_types_impl globs a1) (fun e1 ->
    MonError.bind (transl_atom abs_types_impl globs a2) (fun e2 ->
      MonError.bind (transl_atom abs_types_impl globs a3) (fun e3 ->
        let tarith = typeof e1 in
        let tderef = typeof e3 in
        MonError.ret (e1, (Sassign ((Ederef ((Ebinop (Oadd, e1, e2, tarith)),
          tderef)), e3))))))
| EcRecordUpdate (a1, f, a2) ->
  MonError.bind (transl_atom abs_types_impl globs a1) (fun e1 ->
    MonError.bind (transl_atom abs_types_impl globs a2) (fun e2 ->
      let tderef = deref_pointer (typeof e1) in
      let tfield = typeof e2 in
      MonError.ret (e1, (Sassign ((Efield ((Ederef (e1, tderef)), (to_pos f),
        tfield)), e2)))))

(** val transl_cond_atom :
    coq_type PMap.t -> PositiveSet.t -> Imp2.atom -> cexpr res **)

let rec transl_cond_atom abs_types_impl globs a =
  match transl_atom abs_types_impl globs a with
  | OK a' -> MonError.ret (CE_expr a')
  | Error _ ->
    (match a with
     | Typed.AUnaryOp (op, a1, _) ->
       MonError.bind (transl_cond_atom abs_types_impl globs a1) (fun a1' ->
         match op with
         | UopNotbool -> MonError.ret (CE_not a1')
         | _ -> MonError.fail)
     | Typed.ABinaryOp (op, a1, a2, _) ->
       MonError.bind (transl_cond_atom abs_types_impl globs a1) (fun a1' ->
         MonError.bind (transl_cond_atom abs_types_impl globs a2) (fun a2' ->
           match op with
           | BopAndbool -> MonError.ret (CE_and (a1', a2'))
           | BopOrbool -> MonError.ret (CE_or (a1', a2'))
           | _ -> MonError.fail))
     | _ -> MonError.fail)

(** val transl_statement :
    coq_type PMap.t -> PositiveSet.t -> statement -> ClightCe.statement res **)

let rec transl_statement abs_types_impl globs = function
| StSkip -> MonError.ret Sskip
| StSetExpr (x, e) ->
  MonError.bind (transl_expr abs_types_impl globs e) (fun e' ->
    MonError.ret (Sset ((to_pos x), e')))
| StSetEcomp (x, ec) ->
  MonError.bind2 (transl_ecomp abs_types_impl globs ec) (fun e s0 ->
    let sset = Sset ((to_pos x), e) in MonError.ret (Ssequence (s0, sset)))
| StCall (x, a, args) ->
  MonError.bind (transl_atom abs_types_impl globs a) (fun e ->
    MonError.bind (mmap (transl_atom abs_types_impl globs) args)
      (fun args' -> MonError.ret (Scall ((Some (to_pos x)), e, args'))))
| StIfThenElse (a, s1, s2) ->
  MonError.bind (transl_cond_atom abs_types_impl globs a) (fun e ->
    MonError.bind (transl_statement abs_types_impl globs s1) (fun s1' ->
      MonError.bind (transl_statement abs_types_impl globs s2) (fun s2' ->
        MonError.ret (Sifthenelse (e, s1', s2')))))
| StSequence (s1, s2) ->
  MonError.bind (transl_statement abs_types_impl globs s1) (fun s1' ->
    MonError.bind (transl_statement abs_types_impl globs s2) (fun s2' ->
      MonError.ret (Ssequence (s1', s2'))))
| StReturn a ->
  MonError.bind (transl_atom abs_types_impl globs a) (fun e ->
    MonError.ret (Sreturn (Some e)))

(** val transl_function :
    coq_type PMap.t -> PositiveSet.t -> Imp2.coq_function ->
    ClightCe.coq_function res **)

let transl_function abs_types_impl globs f =
  let ty = transl_btyp abs_types_impl f.Imp2.fn_return in
  let params =
    map (fun pat ->
      let (pid, pty) = pat in ((to_pos pid), (transl_btyp abs_types_impl pty)))
      f.Imp2.fn_params
  in
  let temps =
    map (fun pat ->
      let (pid, pty) = pat in ((to_pos pid), (transl_btyp abs_types_impl pty)))
      f.fn_vars
  in
  MonError.bind (transl_statement abs_types_impl globs f.Imp2.fn_body)
    (fun body ->
    MonError.ret { ClightCe.fn_return = ty; fn_callconv = cc_default;
      ClightCe.fn_params = params; ClightCe.fn_vars = []; fn_temps = temps;
      ClightCe.fn_body = body })

(** val transl_abs_function :
    coq_type PMap.t -> Syntax.ident -> (param_attr * btyp) list -> btyp ->
    ClightCe.fundef **)

let transl_abs_function abs_types_impl f tparams tret =
  let tparams0 = map snd tparams in
  let ext_func = EF_external (f, { sig_args =
    (map transl_btyp_to_xtype tparams0); sig_res =
    (transl_btyp_to_xtype tret); sig_cc = cc_default })
  in
  External (ext_func, (map (transl_btyp abs_types_impl) tparams0),
  (transl_btyp abs_types_impl tret), cc_default)

type cglobdef =
  AST.ident * (ClightCe.coq_function fundef, coq_type) AST.globdef

(** val literal_size : Imp2.literal -> coq_Z **)

let literal_size = function
| Imp2.LArray (a, _) -> Z.of_nat (Barray.length a)
| _ -> Z.of_nat O

(** val transl_globdefs_rec :
    coq_type PMap.t -> Imp2.globdef list -> PositiveSet.t -> cglobdef list res **)

let rec transl_globdefs_rec abs_types_impl defs globs =
  match defs with
  | [] -> MonError.ret []
  | d :: defs' ->
    (match d with
     | DefConst (x, l, ty) ->
       let init0 = transl_literal l in
       let t0 = transl_btyp_lit abs_types_impl ty (literal_size l) in
       let t1 =
         match ty with
         | BRecord _ ->
           (match l with
            | LBase (l0, _) -> (match l0 with
                                | LbVar _ -> tptr t0
                                | _ -> t0)
            | _ -> t0)
         | _ -> t0
       in
       let d0 = ((to_pos x), (Gvar { gvar_info = t1; gvar_init = init0;
         gvar_readonly = (btyp_is_prim ty); gvar_volatile = false }))
       in
       MonError.bind
         (transl_globdefs_rec abs_types_impl defs' (sadd globs (to_pos x)))
         (fun r -> MonError.ret (d0 :: r))
     | DefFun (x, f) ->
       MonError.bind (transl_function abs_types_impl globs f) (fun f' ->
         let d0 = ((to_pos x), (Gfun (Internal f'))) in
         MonError.bind
           (transl_globdefs_rec abs_types_impl defs' (sadd globs (to_pos x)))
           (fun r -> MonError.ret (d0 :: r)))
     | DeclConst (x, ty) ->
       let t0 = transl_btyp abs_types_impl ty in
       MonError.bind
         (transl_globdefs_rec abs_types_impl defs' (sadd globs (to_pos x)))
         (fun r ->
         let d0 = ((to_pos x), (Gvar { gvar_info = t0; gvar_init = [];
           gvar_readonly = false; gvar_volatile = false }))
         in
         MonError.ret (d0 :: r))
     | DeclFun (x, tparams, tret) ->
       MonError.bind
         (transl_globdefs_rec abs_types_impl defs' (sadd globs (to_pos x)))
         (fun r ->
         let d0 = ((to_pos x), (Gfun
           (transl_abs_function abs_types_impl x tparams tret)))
         in
         MonError.ret (d0 :: r)))

(** val transl_globdefs :
    coq_type PMap.t -> Imp2.globdef list -> cglobdef list res **)

let transl_globdefs abs_types_impl defs =
  transl_globdefs_rec abs_types_impl defs PositiveSet.empty

(** val transl_record_fields :
    coq_type PMap.t -> btyp SMapList.t -> members **)

let rec transl_record_fields abs_types_impl = function
| [] -> []
| p :: fields' ->
  let (x, tx) = p in
  let tx' = transl_btyp abs_types_impl tx in
  let r = transl_record_fields abs_types_impl fields' in
  (Member_plain ((to_pos x), tx')) :: r

(** val transl_struct_btyp :
    coq_type PMap.t -> Syntax.ident -> btyp SMapList.t -> composite_definition **)

let transl_struct_btyp abs_types_impl x fields =
  Composite ((to_pos x), Struct,
    (transl_record_fields abs_types_impl fields), noattr)

(** val transl_prog_types :
    coq_type PMap.t -> type_def list -> composite_definition list **)

let transl_prog_types abs_types_impl types =
  map (fun sd -> transl_struct_btyp abs_types_impl sd.rd_name sd.rd_fields)
    (get_record_defs types)

(** val public_idents : Imp2.globdef list -> Syntax.ident list **)

let rec public_idents = function
| [] -> []
| d :: defs' ->
  (match d with
   | DefConst (x, _, _) -> x :: (public_idents defs')
   | DefFun (x, _) -> x :: (public_idents defs')
   | DeclConst (x, _) -> x :: (public_idents defs')
   | DeclFun (x, _, _) -> x :: (public_idents defs'))

(** val _main : AST.ident **)

let _main =
  Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO
    (Coq_xI (Coq_xI (Coq_xI (Coq_xO (Coq_xI (Coq_xO
    Coq_xH)))))))))))))))))))))))

(** val mk_abs_types_impl : type_def list -> coq_type PMap.t **)

let rec mk_abs_types_impl = function
| [] -> PMap.init Tvoid
| td :: types' ->
  (match td with
   | TdRecord _ -> mk_abs_types_impl types'
   | TdAbstract (tid, tk) ->
     let ct =
       match tk with
       | Struct -> Tstruct ((to_pos tid), noattr)
       | Union -> Tunion ((to_pos tid), noattr)
     in
     PMap.set (to_pos tid) ct (mk_abs_types_impl types'))

(** val transl_program : Imp2.program -> ClightCe.program res **)

let transl_program prog =
  let types = prog.prog_types in
  let defs = prog.prog_defs in
  let abs_types_impl = mk_abs_types_impl types in
  let ts = transl_prog_types abs_types_impl types in
  MonError.bind (transl_globdefs abs_types_impl defs) (fun cdefs ->
    let public = map to_pos (public_idents defs) in
    (match make_program ts cdefs public _main with
     | OK prog0 -> OK prog0
     | Error _ ->
       MonError.failwith
         ('C'::('l'::('i'::('g'::('h'::('t'::('g'::('e'::('n'::('C'::('e'::('.'::('t'::('r'::('a'::('n'::('s'::('l'::('_'::('p'::('r'::('o'::('g'::('r'::('a'::('m'::(':'::(' '::('e'::('r'::('r'::('o'::('r'::(' '::('w'::('h'::('e'::('n'::(' '::('c'::('a'::('l'::('l'::('i'::('n'::('g'::(' '::('C'::('t'::('y'::('p'::('e'::('s'::('.'::('m'::('a'::('k'::('e'::('_'::('p'::('r'::('o'::('g'::('r'::('a'::('m'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val typ_idents_rec : PositiveSet.t -> coq_type -> PositiveSet.t **)

let rec typ_idents_rec ids = function
| Tpointer (t', _) -> sunion ids (typ_idents_rec ids t')
| Tarray (t', _, _) -> sunion ids (typ_idents_rec ids t')
| Tfunction (tparams, tret, _) ->
  fold_left typ_idents_rec tparams (typ_idents_rec ids tret)
| Tstruct (tid, _) -> sadd ids tid
| Tunion (tid, _) -> sadd ids tid
| _ -> ids

(** val typ_idents : coq_type -> PositiveSet.t **)

let typ_idents t0 =
  typ_idents_rec PositiveSet.empty t0

(** val function_idents : ClightCe.coq_function -> PositiveSet.t **)

let function_idents f =
  let param_idents =
    fold_left (fun ids pat ->
      let (pid, pty) = pat in sunion (typ_idents pty) (sadd ids pid))
      f.ClightCe.fn_params PositiveSet.empty
  in
  let var_idents =
    fold_left sadd (map fst f.ClightCe.fn_vars) PositiveSet.empty
  in
  let temp_idents = fold_left sadd (map fst f.fn_temps) PositiveSet.empty in
  sunion (sunion param_idents var_idents) temp_idents

(** val globdef_idents : cglobdef -> PositiveSet.t **)

let globdef_idents = function
| (x, def0) ->
  let ids =
    match def0 with
    | Gfun f0 ->
      (match f0 with
       | Internal f -> function_idents f
       | External (_, tparams, tret, _) ->
         fold_left (fun ids pty -> sunion ids (typ_idents pty)) tparams
           (typ_idents tret))
    | Gvar _ -> PositiveSet.empty
  in
  sadd ids x

(** val members_idents : members -> PositiveSet.t **)

let rec members_idents = function
| [] -> PositiveSet.empty
| h :: t0 -> sadd (members_idents t0) (name_member h)

(** val composite_idents : composite_definition -> PositiveSet.t **)

let composite_idents = function
| Composite (x, _, m, _) -> sadd (members_idents m) x

(** val program_idents : ClightCe.program -> AST.ident list **)

let program_idents prog =
  let ids =
    fold_left (fun ids def -> sunion (globdef_idents def) ids)
      prog.Ctypes.prog_defs PositiveSet.empty
  in
  let ids0 =
    fold_left (fun ids0 types -> sunion (composite_idents types) ids0)
      prog.Ctypes.prog_types ids
  in
  PositiveSet.elements ids0
