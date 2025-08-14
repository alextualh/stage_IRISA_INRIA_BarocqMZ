open Errors

module MonError :
 sig
  type 'x coq_M = 'x res

  val ret : 'a1 -> 'a1 coq_M

  val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M

  val bind2 : ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M

  val fail : 'a1 coq_M

  val err_of_opt : 'a1 option -> 'a1 coq_M
 end
