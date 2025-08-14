open Brecord
open Errors
open Ident
open List0
open Maps2
open Target

type __ = Obj.t

type ident = t

type signedness =
| Signed
| Unsigned

val signedness_eq_dec : signedness -> signedness -> bool

type typ =
| TBool
| TInt32 of signedness
| TInt64 of signedness
| TArray of typ
| TRecord of ident * (ident * typ) list
| TFun of typ list * typ
| TAbs of ident

val typ_eq_dec : typ -> typ -> bool

type btyp =
| BBool
| BInt32 of signedness
| BInt64 of signedness
| BArray of btyp
| BRecord of ident
| BFun of btyp list * btyp
| BAbs of ident

val btyp_is_prim : btyp -> bool

val btyp_eq_dec : btyp -> btyp -> bool

val mk_fun_btyp : ('a1 * btyp) list -> btyp -> btyp

val typof_field : ident -> typ SMapList.t -> typ res

val btypof_field : ident -> btyp SMapList.t -> btyp res

val arr_index_btyp : archi -> btyp

val eval_fields_typ : typ SMapList.t -> __ SMapList.t

type 'eval_typ eval_recordtyp = record

type 'eval_typ eval_funtyp = __

type eval_typ = __

val typ_cast : typ -> typ -> __ SMap.t -> eval_typ -> eval_typ
