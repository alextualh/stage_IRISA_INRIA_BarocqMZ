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

val update : __ SMapList.t -> record -> key -> type_of_field -> record res
