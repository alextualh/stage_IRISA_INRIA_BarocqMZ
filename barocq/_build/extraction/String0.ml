open Ascii
open Datatypes

(** val string_dec : char list -> char list -> bool **)

let rec string_dec s x =
  match s with
  | [] -> (match x with
           | [] -> true
           | _::_ -> false)
  | a::s0 ->
    (match x with
     | [] -> false
     | a0::s1 -> if (=) a a0 then string_dec s0 s1 else false)

(** val compare : char list -> char list -> comparison **)

let rec compare s1 s2 =
  match s1 with
  | [] -> (match s2 with
           | [] -> Eq
           | _::_ -> Lt)
  | c1::s1' ->
    (match s2 with
     | [] -> Gt
     | c2::s2' ->
       (match Ascii.compare c1 c2 with
        | Eq -> compare s1' s2'
        | x -> x))

(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)
