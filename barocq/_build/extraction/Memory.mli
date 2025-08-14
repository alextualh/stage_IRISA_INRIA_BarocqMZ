open BinInt
open BinNums
open Coqlib
open Maps
open Memdata
open Memtype
open Values

module Mem :
 sig
  type mem' = { mem_contents : memval ZMap.t PMap.t;
                mem_access : (coq_Z -> perm_kind -> permission option) PMap.t;
                nextblock : block }

  val mem_access : mem' -> (coq_Z -> perm_kind -> permission option) PMap.t

  type mem = mem'

  val perm_order_dec : permission -> permission -> bool

  val perm_order'_dec : permission option -> permission -> bool

  val perm_dec : mem -> block -> coq_Z -> perm_kind -> permission -> bool

  val valid_pointer : mem -> block -> coq_Z -> bool

  val weak_valid_pointer : mem -> block -> coq_Z -> bool

  val empty : mem
 end
