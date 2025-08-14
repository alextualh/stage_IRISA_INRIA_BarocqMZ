open Datatypes
open Errors
open Integers
open Intop
open List0
open Monads
open PeanoNat

type 'a array = 'a list

(** val length : 'a1 array -> int **)

let length =
  length

(** val valid_index : 'a1 array -> Int64.int -> bool **)

let valid_index a i =
  (&&) (Int64.cmpu Cle Int64.zero i) (Nat.ltb (U64.to_nat i) (length a))

(** val get : 'a1 array -> Int64.int -> 'a1 res **)

let get a i =
  if valid_index a i
  then MonError.err_of_opt (nth_error a (U64.to_nat i))
  else MonError.fail

(** val set_rec : 'a1 list -> int -> 'a1 -> 'a1 list **)

let rec set_rec l n x =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match l with
              | [] -> []
              | _ :: l' -> x :: l')
    (fun n' -> match l with
               | [] -> []
               | y :: l' -> y :: (set_rec l' n' x))
    n

(** val set : 'a1 array -> Int64.int -> 'a1 -> 'a1 array res **)

let set a i x =
  if valid_index a i
  then MonError.ret (set_rec a (U64.to_nat i) x)
  else MonError.fail
