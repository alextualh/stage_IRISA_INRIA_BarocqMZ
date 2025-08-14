open Archi
open BinInt
open Integers
open List0

(** val bytes_of_int : int -> Stdlib.Int.t -> Byte.int list **)

let rec bytes_of_int n x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun m ->
    (Byte.repr x) :: (bytes_of_int m
                       (Z.div x ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                         ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
                         ((fun p->2*p) ((fun p->2*p) 1)))))))))))
    n

(** val int_of_bytes : Byte.int list -> Stdlib.Int.t **)

let rec int_of_bytes = function
| [] -> 0
| b :: l' ->
  Z.add (Byte.unsigned b)
    (Z.mul (int_of_bytes l') ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
      1)))))))))

(** val rev_if_be : Byte.int list -> Byte.int list **)

let rev_if_be l =
  if big_endian then rev l else l

(** val encode_int : int -> Stdlib.Int.t -> Byte.int list **)

let encode_int sz x =
  rev_if_be (bytes_of_int sz x)

(** val decode_int : Byte.int list -> Stdlib.Int.t **)

let decode_int b =
  int_of_bytes (rev_if_be b)
