open Archi
open BinInt
open BinNums
open Bool
open Coqlib
open Errors
open Floats
open Integers
open List0

type ident = positive

(** val ident_eq : positive -> positive -> bool **)

let ident_eq =
  peq

type typ =
| Tint
| Tfloat
| Tlong
| Tsingle
| Tany32
| Tany64

(** val coq_Tptr : typ **)

let coq_Tptr =
  if ptr64 then Tlong else Tint

type xtype =
| Xbool
| Xint8signed
| Xint8unsigned
| Xint16signed
| Xint16unsigned
| Xint
| Xfloat
| Xlong
| Xsingle
| Xptr
| Xany32
| Xany64
| Xvoid

(** val coq_Xsize_t : xtype **)

let coq_Xsize_t =
  if ptr64 then Xlong else Xint

(** val xtype_eq : xtype -> xtype -> bool **)

let xtype_eq t1 t2 =
  match t1 with
  | Xbool -> (match t2 with
              | Xbool -> true
              | _ -> false)
  | Xint8signed -> (match t2 with
                    | Xint8signed -> true
                    | _ -> false)
  | Xint8unsigned -> (match t2 with
                      | Xint8unsigned -> true
                      | _ -> false)
  | Xint16signed -> (match t2 with
                     | Xint16signed -> true
                     | _ -> false)
  | Xint16unsigned -> (match t2 with
                       | Xint16unsigned -> true
                       | _ -> false)
  | Xint -> (match t2 with
             | Xint -> true
             | _ -> false)
  | Xfloat -> (match t2 with
               | Xfloat -> true
               | _ -> false)
  | Xlong -> (match t2 with
              | Xlong -> true
              | _ -> false)
  | Xsingle -> (match t2 with
                | Xsingle -> true
                | _ -> false)
  | Xptr -> (match t2 with
             | Xptr -> true
             | _ -> false)
  | Xany32 -> (match t2 with
               | Xany32 -> true
               | _ -> false)
  | Xany64 -> (match t2 with
               | Xany64 -> true
               | _ -> false)
  | Xvoid -> (match t2 with
              | Xvoid -> true
              | _ -> false)

(** val inj_type : typ -> xtype **)

let inj_type = function
| Tint -> Xint
| Tfloat -> Xfloat
| Tlong -> Xlong
| Tsingle -> Xsingle
| Tany32 -> Xany32
| Tany64 -> Xany64

type calling_convention = { cc_vararg : coq_Z option; cc_unproto : bool;
                            cc_structret : bool }

(** val cc_default : calling_convention **)

let cc_default =
  { cc_vararg = None; cc_unproto = false; cc_structret = false }

(** val calling_convention_eq :
    calling_convention -> calling_convention -> bool **)

let calling_convention_eq x y =
  let { cc_vararg = cc_vararg0; cc_unproto = cc_unproto0; cc_structret =
    cc_structret0 } = x
  in
  let { cc_vararg = cc_vararg1; cc_unproto = cc_unproto1; cc_structret =
    cc_structret1 } = y
  in
  if match cc_vararg0 with
     | Some a ->
       (match cc_vararg1 with
        | Some z -> Z.eq_dec a z
        | None -> false)
     | None -> (match cc_vararg1 with
                | Some _ -> false
                | None -> true)
  then if bool_dec cc_unproto0 cc_unproto1
       then bool_dec cc_structret0 cc_structret1
       else false
  else false

type signature = { sig_args : xtype list; sig_res : xtype;
                   sig_cc : calling_convention }

type memory_chunk =
| Mbool
| Mint8signed
| Mint8unsigned
| Mint16signed
| Mint16unsigned
| Mint32
| Mint64
| Mfloat32
| Mfloat64
| Many32
| Many64

(** val coq_Mptr : memory_chunk **)

let coq_Mptr =
  if ptr64 then Mint64 else Mint32

(** val xtype_of_chunk : memory_chunk -> xtype **)

let xtype_of_chunk = function
| Mbool -> Xbool
| Mint8signed -> Xint8signed
| Mint8unsigned -> Xint8unsigned
| Mint16signed -> Xint16signed
| Mint16unsigned -> Xint16unsigned
| Mint32 -> Xint
| Mint64 -> Xlong
| Mfloat32 -> Xsingle
| Mfloat64 -> Xfloat
| Many32 -> Xany32
| Many64 -> Xany64

type init_data =
| Init_int8 of Int.int
| Init_int16 of Int.int
| Init_int32 of Int.int
| Init_int64 of Int64.int
| Init_float32 of float32
| Init_float64 of float
| Init_space of coq_Z
| Init_addrof of ident * Ptrofs.int

(** val init_data_size : init_data -> coq_Z **)

let init_data_size = function
| Init_int8 _ -> Zpos Coq_xH
| Init_int16 _ -> Zpos (Coq_xO Coq_xH)
| Init_int32 _ -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Init_float32 _ -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Init_space n -> Z.max n Z0
| Init_addrof (_, _) ->
  if ptr64
  then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
  else Zpos (Coq_xO (Coq_xO Coq_xH))
| _ -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

type 'v globvar = { gvar_info : 'v; gvar_init : init_data list;
                    gvar_readonly : bool; gvar_volatile : bool }

type ('f, 'v) globdef =
| Gfun of 'f
| Gvar of 'v globvar

type ('f, 'v) program = { prog_defs : (ident * ('f, 'v) globdef) list;
                          prog_public : ident list; prog_main : ident }

(** val transf_globvar :
    (ident -> 'a1 -> 'a2 res) -> ident -> 'a1 globvar -> 'a2 globvar res **)

let transf_globvar transf_var i g =
  match transf_var i g.gvar_info with
  | OK x ->
    OK { gvar_info = x; gvar_init = g.gvar_init; gvar_readonly =
      g.gvar_readonly; gvar_volatile = g.gvar_volatile }
  | Error msg -> Error msg

(** val transf_globdefs :
    (ident -> 'a1 -> 'a2 res) -> (ident -> 'a3 -> 'a4 res) -> (ident * ('a1,
    'a3) globdef) list -> (ident * ('a2, 'a4) globdef) list res **)

let rec transf_globdefs transf_fun transf_var = function
| [] -> OK []
| p :: l' ->
  let (id, g) = p in
  (match g with
   | Gfun f ->
     (match transf_fun id f with
      | OK tf ->
        (match transf_globdefs transf_fun transf_var l' with
         | OK x -> OK ((id, (Gfun tf)) :: x)
         | Error msg -> Error msg)
      | Error msg ->
        Error ((MSG
          ('I'::('n'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::[]))))))))))))) :: ((CTX
          id) :: ((MSG (':'::(' '::[]))) :: msg))))
   | Gvar v ->
     (match transf_globvar transf_var id v with
      | OK tv ->
        (match transf_globdefs transf_fun transf_var l' with
         | OK x -> OK ((id, (Gvar tv)) :: x)
         | Error msg -> Error msg)
      | Error msg ->
        Error ((MSG
          ('I'::('n'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::[]))))))))))))) :: ((CTX
          id) :: ((MSG (':'::(' '::[]))) :: msg)))))

(** val transform_partial_program2 :
    (ident -> 'a1 -> 'a2 res) -> (ident -> 'a3 -> 'a4 res) -> ('a1, 'a3)
    program -> ('a2, 'a4) program res **)

let transform_partial_program2 transf_fun transf_var p =
  match transf_globdefs transf_fun transf_var p.prog_defs with
  | OK x ->
    OK { prog_defs = x; prog_public = p.prog_public; prog_main = p.prog_main }
  | Error msg -> Error msg

(** val transform_partial_program :
    ('a1 -> 'a2 res) -> ('a1, 'a3) program -> ('a2, 'a3) program res **)

let transform_partial_program transf_fun p =
  transform_partial_program2 (fun _ -> transf_fun) (fun _ v -> OK v) p

type external_function =
| EF_external of char list * signature
| EF_builtin of char list * signature
| EF_runtime of char list * signature
| EF_vload of memory_chunk
| EF_vstore of memory_chunk
| EF_malloc
| EF_free
| EF_memcpy of coq_Z * coq_Z
| EF_annot of positive * char list * typ list
| EF_annot_val of positive * char list * typ
| EF_inline_asm of char list * signature * char list list
| EF_debug of positive * ident * typ list

(** val ef_sig : external_function -> signature **)

let ef_sig = function
| EF_external (_, sg) -> sg
| EF_builtin (_, sg) -> sg
| EF_runtime (_, sg) -> sg
| EF_vload chunk ->
  { sig_args = (Xptr :: []); sig_res = (xtype_of_chunk chunk); sig_cc =
    cc_default }
| EF_vstore chunk ->
  { sig_args = (Xptr :: ((xtype_of_chunk chunk) :: [])); sig_res = Xvoid;
    sig_cc = cc_default }
| EF_malloc ->
  { sig_args = (coq_Xsize_t :: []); sig_res = Xptr; sig_cc = cc_default }
| EF_free -> { sig_args = (Xptr :: []); sig_res = Xvoid; sig_cc = cc_default }
| EF_memcpy (_, _) ->
  { sig_args = (Xptr :: (Xptr :: [])); sig_res = Xvoid; sig_cc = cc_default }
| EF_annot (_, _, targs) ->
  { sig_args = (map inj_type targs); sig_res = Xvoid; sig_cc = cc_default }
| EF_annot_val (_, _, targ) ->
  { sig_args = ((inj_type targ) :: []); sig_res = (inj_type targ); sig_cc =
    cc_default }
| EF_inline_asm (_, sg, _) -> sg
| EF_debug (_, _, targs) ->
  { sig_args = (map inj_type targs); sig_res = Xvoid; sig_cc = cc_default }

type 'a builtin_arg =
| BA of 'a
| BA_int of Int.int
| BA_long of Int64.int
| BA_float of float
| BA_single of float32
| BA_loadstack of memory_chunk * Ptrofs.int
| BA_addrstack of Ptrofs.int
| BA_loadglobal of memory_chunk * ident * Ptrofs.int
| BA_addrglobal of ident * Ptrofs.int
| BA_splitlong of 'a builtin_arg * 'a builtin_arg
| BA_addptr of 'a builtin_arg * 'a builtin_arg

type 'a builtin_res =
| BR of 'a
| BR_none
| BR_splitlong of 'a builtin_res * 'a builtin_res
