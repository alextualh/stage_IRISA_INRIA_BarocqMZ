open Compare_dec
open Datatypes
open Error
open Integers
open Nat

type mem_val = Byte.int

type mem = mem_val list

type mem_addr = int

(** val valid_access_dec : mem -> mem_addr -> int -> bool **)

let valid_access_dec m addr s =
  let s0 = (<) addr (length m) in
  if s0
  then let s1 = (<) 0 s in if s1 then (<) (add addr s) (length m) else false
  else false

(** val read_aux : mem -> mem_addr -> int -> mem_val list option **)

let rec read_aux m addr s =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> Some [])
      (fun s' ->
      match m with
      | [] -> None
      | b :: m' -> bind (read_aux m' 0 s') (fun lv -> Some (b :: lv)))
      s)
    (fun addr' ->
    match m with
    | [] -> None
    | _ :: m' -> read_aux m' addr' s)
    addr

(** val read : mem -> mem_addr -> int -> mem_val list option **)

let read m addr s =
  if valid_access_dec m addr s then read_aux m addr s else None

(** val write_aux : mem -> mem_addr -> mem_val list -> mem option **)

let rec write_aux m addr lv =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    match m with
    | [] -> (match lv with
             | [] -> Some m
             | _ :: _ -> None)
    | _ :: m' ->
      (match lv with
       | [] -> Some m
       | v :: lv' -> bind (write_aux m' 0 lv') (fun r -> Some (v :: r))))
    (fun addr' ->
    match m with
    | [] -> None
    | b :: m' -> bind (write_aux m' addr' lv) (fun r -> Some (b :: r)))
    addr

(** val write : mem -> mem_addr -> mem_val list -> mem option **)

let write m addr lv =
  if valid_access_dec m addr (length lv) then write_aux m addr lv else None

(** val access_overlap_dec : mem_addr -> int -> mem_addr -> int -> bool **)

let access_overlap_dec addr s addr' s' =
  let s0 = le_dec addr' (add addr (sub s (Stdlib.Int.succ 0))) in
  if s0 then le_dec addr (add addr' (sub s' (Stdlib.Int.succ 0))) else false

(** val access_contained : mem_addr -> int -> mem_addr -> int -> bool **)

let access_contained addr s addr' s' =
  (&&) ((<=) addr' addr) ((<=) (add addr s) (add addr' s'))
