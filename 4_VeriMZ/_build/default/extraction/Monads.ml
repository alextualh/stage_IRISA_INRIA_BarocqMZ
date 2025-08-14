open Errors

module MonError =
 struct
  type 'x coq_M = 'x res

  (** val ret : 'a1 -> 'a1 coq_M **)

  let ret a =
    OK a

  (** val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M **)

  let bind =
    bind

  (** val bind2 :
      ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M **)

  let bind2 =
    bind2

  (** val fail : 'a1 coq_M **)

  let fail =
    Error []

  (** val err_of_opt : 'a1 option -> 'a1 coq_M **)

  let err_of_opt = function
  | Some v -> OK v
  | None -> fail
 end
