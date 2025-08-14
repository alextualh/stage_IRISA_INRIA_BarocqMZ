open BinInt
open BinNums
open Coqlib
open Maps
open Memdata
open Memtype
open Values

module Mem =
 struct
  type mem' = { mem_contents : memval ZMap.t PMap.t;
                mem_access : (coq_Z -> perm_kind -> permission option) PMap.t;
                nextblock : block }

  (** val mem_access :
      mem' -> (coq_Z -> perm_kind -> permission option) PMap.t **)

  let mem_access m =
    m.mem_access

  type mem = mem'

  (** val perm_order_dec : permission -> permission -> bool **)

  let perm_order_dec p1 p2 =
    match p1 with
    | Freeable -> true
    | Writable -> (match p2 with
                   | Freeable -> false
                   | _ -> true)
    | Readable ->
      (match p2 with
       | Freeable -> false
       | Writable -> false
       | _ -> true)
    | Nonempty -> (match p2 with
                   | Nonempty -> true
                   | _ -> false)

  (** val perm_order'_dec : permission option -> permission -> bool **)

  let perm_order'_dec op p =
    match op with
    | Some p0 -> perm_order_dec p0 p
    | None -> false

  (** val perm_dec :
      mem -> block -> coq_Z -> perm_kind -> permission -> bool **)

  let perm_dec m b ofs k p =
    perm_order'_dec (PMap.get b m.mem_access ofs k) p

  (** val valid_pointer : mem -> block -> coq_Z -> bool **)

  let valid_pointer m b ofs =
    proj_sumbool (perm_dec m b ofs Cur Nonempty)

  (** val weak_valid_pointer : mem -> block -> coq_Z -> bool **)

  let weak_valid_pointer m b ofs =
    (||) (valid_pointer m b ofs) (valid_pointer m b (Z.sub ofs (Zpos Coq_xH)))

  (** val empty : mem **)

  let empty =
    { mem_contents = (PMap.init (ZMap.init Undef)); mem_access =
      (PMap.init (fun _ _ -> None)); nextblock = Coq_xH }
 end
