open BinNums
open Datatypes
open Errors
open Ident
open List0
open MSetPositive
open Monads

(** val fresh_var : char list -> t MonCounter.coq_M **)

let fresh_var prefix =
  MonCounter.bind MonCounter.get (fun ctr ->
    let var = prefix_with prefix (of_str_nat ctr) in MonCounter.incr var)

(** val fresh_var_err : char list -> t MonCounterErr.coq_M **)

let fresh_var_err prefix =
  MonCounterErr.bind MonCounterErr.get (fun ctr ->
    let var = prefix_with prefix (of_str_nat ctr) in MonCounterErr.incr var)

(** val nth_err : 'a1 list -> nat -> 'a1 res **)

let nth_err l n =
  MonError.err_of_opt (nth_error l n)

(** val fold_left_err :
    ('a1 -> 'a2 -> 'a1 res) -> 'a2 list -> 'a1 res -> 'a1 res **)

let rec fold_left_err f l a0 =
  match l with
  | [] -> a0
  | x :: l' -> MonError.bind a0 (fun a1 -> fold_left_err f l' (f a1 x))

(** val list_is_empty : 'a1 list -> bool **)

let list_is_empty = function
| [] -> true
| _ :: _ -> false

(** val smem : PositiveSet.t -> positive -> bool **)

let smem s p =
  PositiveSet.mem p s

(** val sadd : PositiveSet.t -> positive -> PositiveSet.t **)

let sadd s p =
  PositiveSet.add p s

(** val sunion : PositiveSet.t -> PositiveSet.t -> PositiveSet.t **)

let sunion =
  PositiveSet.union
