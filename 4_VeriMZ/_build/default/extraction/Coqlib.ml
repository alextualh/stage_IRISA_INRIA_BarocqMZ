open BinInt
open ZArith_dec

(** val zeq : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

let zlt =
  coq_Z_lt_dec

(** val zle : Stdlib.Int.t -> Stdlib.Int.t -> bool **)

let zle =
  coq_Z_le_gt_dec

(** val proj_sumbool : bool -> bool **)

let proj_sumbool = function
| true -> true
| false -> false
