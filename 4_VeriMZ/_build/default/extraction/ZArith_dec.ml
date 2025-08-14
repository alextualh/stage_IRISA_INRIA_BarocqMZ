open BinInt
open Datatypes

(** val coq_Z_lt_dec : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

let coq_Z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val coq_Z_le_dec : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

let coq_Z_le_dec x y =
  match Z.compare x y with
  | Gt -> false
  | _ -> true

(** val coq_Z_le_gt_dec : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

let coq_Z_le_gt_dec =
  coq_Z_le_dec
