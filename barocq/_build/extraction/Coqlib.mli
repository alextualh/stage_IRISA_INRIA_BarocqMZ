open BinInt
open BinNums
open BinPos
open ZArith_dec

val peq : positive -> positive -> bool

val zeq : coq_Z -> coq_Z -> bool

val zlt : coq_Z -> coq_Z -> bool

val zle : coq_Z -> coq_Z -> bool

val align : coq_Z -> coq_Z -> coq_Z

val floor : coq_Z -> coq_Z -> coq_Z

val option_eq : ('a1 -> 'a1 -> bool) -> 'a1 option -> 'a1 option -> bool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

val proj_sumbool : bool -> bool
