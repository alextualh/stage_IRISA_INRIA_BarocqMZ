open AST
open Archi
open BinInt
open BinNums
open Datatypes
open Integers
open List0
open Values

(** val size_chunk : memory_chunk -> coq_Z **)

let size_chunk = function
| Mbool -> Zpos Coq_xH
| Mint8signed -> Zpos Coq_xH
| Mint8unsigned -> Zpos Coq_xH
| Mint16signed -> Zpos (Coq_xO Coq_xH)
| Mint16unsigned -> Zpos (Coq_xO Coq_xH)
| Mint32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Mfloat32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
| Many32 -> Zpos (Coq_xO (Coq_xO Coq_xH))
| _ -> Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

type quantity =
| Q32
| Q64

type memval =
| Undef
| Byte of Byte.int
| Fragment of coq_val * quantity * nat

(** val bytes_of_int : nat -> coq_Z -> Byte.int list **)

let rec bytes_of_int n x =
  match n with
  | O -> []
  | S m ->
    (Byte.repr x) :: (bytes_of_int m
                       (Z.div x (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                         (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))))))

(** val int_of_bytes : Byte.int list -> coq_Z **)

let rec int_of_bytes = function
| [] -> Z0
| b :: l' ->
  Z.add (Byte.unsigned b)
    (Z.mul (int_of_bytes l') (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))

(** val rev_if_be : Byte.int list -> Byte.int list **)

let rev_if_be l =
  if big_endian then rev l else l

(** val encode_int : nat -> coq_Z -> Byte.int list **)

let encode_int sz x =
  rev_if_be (bytes_of_int sz x)

(** val decode_int : Byte.int list -> coq_Z **)

let decode_int b =
  int_of_bytes (rev_if_be b)
