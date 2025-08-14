open BinInt
open BinPos
open Coqlib

val coq_P_mod_two_p : int -> int -> Stdlib.Int.t

val coq_Zshiftin : bool -> Stdlib.Int.t -> Stdlib.Int.t

val coq_Zzero_ext : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

val coq_Zsign_ext : Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t

val coq_Z_one_bits : int -> Stdlib.Int.t -> Stdlib.Int.t -> Stdlib.Int.t list

val coq_P_is_power2 : int -> bool

val coq_Z_is_power2 : Stdlib.Int.t -> Stdlib.Int.t option

val coq_Zsize : Stdlib.Int.t -> Stdlib.Int.t
