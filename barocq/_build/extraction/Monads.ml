open Datatypes
open Errors
open Nat

module MonError =
 struct
  type 'x coq_M = 'x res

  (** val ret : 'a1 -> 'a1 coq_M **)

  let ret a =
    OK a

  (** val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M **)

  let bind f g =
    match f with
    | OK x -> g x
    | Error msg0 -> Error msg0

  (** val bind_catch :
      'a1 coq_M -> ('a1 -> 'a2 coq_M) -> char list -> 'a2 coq_M **)

  let bind_catch f g m =
    match f with
    | OK a -> g a
    | Error _ -> Error (msg m)

  (** val bind2 :
      ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M **)

  let bind2 f g =
    match f with
    | OK p -> let (x, y) = p in g x y
    | Error msg0 -> Error msg0

  (** val fail : 'a1 coq_M **)

  let fail =
    Error []

  (** val failwith : char list -> 'a1 coq_M **)

  let failwith m =
    Error (msg m)

  (** val err_of_opt : 'a1 option -> 'a1 coq_M **)

  let err_of_opt = function
  | Some v -> OK v
  | None -> fail
 end

module StateCounter =
 struct
  type t = nat
 end

module MonCounter =
 struct
  type 'a coq_M = StateCounter.t -> 'a * StateCounter.t

  (** val ret : 'a1 -> 'a1 coq_M **)

  let ret a s =
    (a, s)

  (** val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M **)

  let bind f g s =
    let (a, s') = f s in g a s'

  (** val bind2 :
      ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M **)

  let bind2 f g s =
    let (p, s') = f s in let (a, b) = p in g a b s'

  (** val get : StateCounter.t coq_M **)

  let get s =
    (s, s)

  (** val incr : 'a1 -> 'a1 coq_M **)

  let incr a s =
    (a, (add s (S O)))
 end

module MonCounterErr =
 struct
  type 'a coq_M = StateCounter.t -> ('a * StateCounter.t) res

  (** val ret : 'a1 -> 'a1 coq_M **)

  let ret a s =
    OK (a, s)

  (** val bind : 'a1 coq_M -> ('a1 -> 'a2 coq_M) -> 'a2 coq_M **)

  let bind f g s =
    match f s with
    | OK p -> let (a, s') = p in g a s'
    | Error msg0 -> Error msg0

  (** val bind2 :
      ('a1 * 'a2) coq_M -> ('a1 -> 'a2 -> 'a3 coq_M) -> 'a3 coq_M **)

  let bind2 f g s =
    match f s with
    | OK p -> let (p0, s') = p in let (a, b) = p0 in g a b s'
    | Error msg0 -> Error msg0

  (** val get : StateCounter.t coq_M **)

  let get s =
    OK (s, s)

  (** val lift_err : 'a1 MonError.coq_M -> 'a1 coq_M **)

  let lift_err f s =
    match f with
    | OK a -> OK (a, s)
    | Error msg0 -> Error msg0

  (** val fail : 'a1 coq_M **)

  let fail _ =
    Error []

  (** val incr : 'a1 -> 'a1 coq_M **)

  let incr a s =
    OK (a, (add s (S O)))
 end
