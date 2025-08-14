open Brecord
open Errors
open Ident
open List0
open Maps2
open Target

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type ident = t

type signedness =
| Signed
| Unsigned

(** val signedness_eq_dec : signedness -> signedness -> bool **)

let signedness_eq_dec s1 s2 =
  match s1 with
  | Signed -> (match s2 with
               | Signed -> true
               | Unsigned -> false)
  | Unsigned -> (match s2 with
                 | Signed -> false
                 | Unsigned -> true)

type typ =
| TBool
| TInt32 of signedness
| TInt64 of signedness
| TArray of typ
| TRecord of ident * (ident * typ) list
| TFun of typ list * typ
| TAbs of ident

(** val typ_eq_dec : typ -> typ -> bool **)

let rec typ_eq_dec t1 t2 =
  match t1 with
  | TBool -> (match t2 with
              | TBool -> true
              | _ -> false)
  | TInt32 s ->
    (match t2 with
     | TInt32 s0 -> signedness_eq_dec s s0
     | _ -> false)
  | TInt64 s ->
    (match t2 with
     | TInt64 s0 -> signedness_eq_dec s s0
     | _ -> false)
  | TArray t0 -> (match t2 with
                  | TArray t3 -> typ_eq_dec t0 t3
                  | _ -> false)
  | TRecord (i, l) ->
    (match t2 with
     | TRecord (i0, l0) ->
       if eq_dec i i0
       then let rec f l1 x =
              match l1 with
              | [] -> (match x with
                       | [] -> true
                       | _ :: _ -> false)
              | y :: l2 ->
                (match x with
                 | [] -> false
                 | p :: l3 ->
                   if let (a, b) = y in
                      let (i1, t0) = p in
                      if eq_dec a i1 then typ_eq_dec b t0 else false
                   then f l2 l3
                   else false)
            in f l l0
       else false
     | _ -> false)
  | TFun (l, t0) ->
    (match t2 with
     | TFun (l0, t3) ->
       if list_eq_dec typ_eq_dec l l0 then typ_eq_dec t0 t3 else false
     | _ -> false)
  | TAbs i -> (match t2 with
               | TAbs i0 -> eq_dec i i0
               | _ -> false)

type btyp =
| BBool
| BInt32 of signedness
| BInt64 of signedness
| BArray of btyp
| BRecord of ident
| BFun of btyp list * btyp
| BAbs of ident

(** val btyp_is_prim : btyp -> bool **)

let btyp_is_prim = function
| BBool -> true
| BInt32 _ -> true
| BInt64 _ -> true
| _ -> false

(** val btyp_eq_dec : btyp -> btyp -> bool **)

let rec btyp_eq_dec t1 t2 =
  match t1 with
  | BBool -> (match t2 with
              | BBool -> true
              | _ -> false)
  | BInt32 s ->
    (match t2 with
     | BInt32 s0 -> signedness_eq_dec s s0
     | _ -> false)
  | BInt64 s ->
    (match t2 with
     | BInt64 s0 -> signedness_eq_dec s s0
     | _ -> false)
  | BArray b -> (match t2 with
                 | BArray b0 -> btyp_eq_dec b b0
                 | _ -> false)
  | BRecord i -> (match t2 with
                  | BRecord i0 -> eq_dec i i0
                  | _ -> false)
  | BFun (l, b) ->
    (match t2 with
     | BFun (l0, b0) ->
       if list_eq_dec btyp_eq_dec l l0 then btyp_eq_dec b b0 else false
     | _ -> false)
  | BAbs i -> (match t2 with
               | BAbs i0 -> eq_dec i i0
               | _ -> false)

(** val mk_fun_btyp : ('a1 * btyp) list -> btyp -> btyp **)

let mk_fun_btyp params tret =
  BFun ((map snd params), tret)

(** val typof_field : ident -> typ SMapList.t -> typ res **)

let typof_field =
  SMapList.find_err

(** val btypof_field : ident -> btyp SMapList.t -> btyp res **)

let btypof_field =
  SMapList.find_err

(** val arr_index_btyp : archi -> btyp **)

let arr_index_btyp = function
| Ptr32 -> BInt32 Unsigned
| Ptr64 -> BInt64 Unsigned

(** val eval_fields_typ : typ SMapList.t -> __ SMapList.t **)

let eval_fields_typ fields =
  SMapList.map (Obj.magic __) fields

type 'eval_typ eval_recordtyp = record

type 'eval_typ eval_funtyp = __

type eval_typ = __

(** val typ_cast : typ -> typ -> __ SMap.t -> eval_typ -> eval_typ **)

let typ_cast _ _ _ x =
  x
