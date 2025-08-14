open Datatypes

val tl : 'a1 list -> 'a1 list

val nth : int -> 'a1 list -> 'a1 -> 'a1

val nth_error : 'a1 list -> int -> 'a1 option

val rev : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list
