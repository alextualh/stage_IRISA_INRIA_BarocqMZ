open Datatypes
open Errors
open Nat

module MonError :
 sig
  type 'x coq_M = 'x res

  val ret : 'a1 -> 'a1 coq_M

  val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M

  val bind_catch : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> char list -> 'a2 coq_M

  val bind2 : ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M

  val fail : 'a1 coq_M

  val failwith : char list -> 'a1 coq_M

  val err_of_opt : 'a1 option -> 'a1 coq_M
 end

module StateCounter :
 sig
  type t = nat
 end

module MonCounter :
 sig
  type 'a coq_M = StateCounter.t -> 'a * StateCounter.t

  val ret : 'a1 -> 'a1 coq_M

  val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M

  val bind2 : ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M

  val get : StateCounter.t coq_M

  val incr : 'a1 -> 'a1 coq_M
 end

module MonCounterErr :
 sig
  type 'a coq_M = StateCounter.t -> ('a * StateCounter.t) res

  val ret : 'a1 -> 'a1 coq_M

  val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M

  val bind2 : ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M

  val get : StateCounter.t coq_M

  val lift_err : 'a1 MonError.coq_M -> 'a1 coq_M

  val fail : 'a1 coq_M

  val incr : 'a1 -> 'a1 coq_M
 end
