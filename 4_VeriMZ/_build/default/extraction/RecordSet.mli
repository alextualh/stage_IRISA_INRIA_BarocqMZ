
type ('r, 't) coq_Setter = ('t -> 't) -> 'r -> 'r

val set : ('a1 -> 'a2) -> ('a1, 'a2) coq_Setter -> ('a2 -> 'a2) -> 'a1 -> 'a1
