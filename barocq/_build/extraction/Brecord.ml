open Errors
open Ident
open Maps2
open Monads

type __ = Obj.t

type key = t

type 'a field = 'a
  (* singleton inductive, whose constructor was Field *)

type type_of_field = __

type record = __

(** val update :
    __ SMapList.t -> record -> key -> type_of_field -> record res **)

let rec update fields rc k v =
  match fields with
  | [] -> MonError.fail
  | p :: l ->
    let (k0, _) = p in
    let (f, r) = Obj.magic rc in
    let s = SMapList.key_eq k0 k in
    if s
    then MonError.ret (Obj.magic (v, r))
    else let r0 = update l r k v in
         (match r0 with
          | OK r1 -> MonError.ret (Obj.magic (f, r1))
          | Error e -> Error e)
