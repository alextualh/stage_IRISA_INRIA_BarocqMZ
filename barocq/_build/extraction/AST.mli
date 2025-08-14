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

val ident_eq : positive -> positive -> bool

type typ =
| Tint
| Tfloat
| Tlong
| Tsingle
| Tany32
| Tany64

val coq_Tptr : typ

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

val coq_Xsize_t : xtype

val xtype_eq : xtype -> xtype -> bool

val inj_type : typ -> xtype

type calling_convention = { cc_vararg : coq_Z option; cc_unproto : bool;
                            cc_structret : bool }

val cc_default : calling_convention

val calling_convention_eq : calling_convention -> calling_convention -> bool

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

val coq_Mptr : memory_chunk

val xtype_of_chunk : memory_chunk -> xtype

type init_data =
| Init_int8 of Int.int
| Init_int16 of Int.int
| Init_int32 of Int.int
| Init_int64 of Int64.int
| Init_float32 of float32
| Init_float64 of float
| Init_space of coq_Z
| Init_addrof of ident * Ptrofs.int

val init_data_size : init_data -> coq_Z

type 'v globvar = { gvar_info : 'v; gvar_init : init_data list;
                    gvar_readonly : bool; gvar_volatile : bool }

type ('f, 'v) globdef =
| Gfun of 'f
| Gvar of 'v globvar

type ('f, 'v) program = { prog_defs : (ident * ('f, 'v) globdef) list;
                          prog_public : ident list; prog_main : ident }

val transf_globvar :
  (ident -> 'a1 -> 'a2 res) -> ident -> 'a1 globvar -> 'a2 globvar res

val transf_globdefs :
  (ident -> 'a1 -> 'a2 res) -> (ident -> 'a3 -> 'a4 res) -> (ident * ('a1,
  'a3) globdef) list -> (ident * ('a2, 'a4) globdef) list res

val transform_partial_program2 :
  (ident -> 'a1 -> 'a2 res) -> (ident -> 'a3 -> 'a4 res) -> ('a1, 'a3)
  program -> ('a2, 'a4) program res

val transform_partial_program :
  ('a1 -> 'a2 res) -> ('a1, 'a3) program -> ('a2, 'a3) program res

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

val ef_sig : external_function -> signature

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
