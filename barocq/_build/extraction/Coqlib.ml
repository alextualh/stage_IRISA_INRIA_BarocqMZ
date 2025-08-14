open BinInt
open BinNums
open BinPos
open ZArith_dec

(** val peq : positive -> positive -> bool **)

let peq =
  Pos.eq_dec

(** val zeq : coq_Z -> coq_Z -> bool **)

let zeq =
  Z.eq_dec

(** val zlt : coq_Z -> coq_Z -> bool **)

let zlt =
  coq_Z_lt_dec

(** val zle : coq_Z -> coq_Z -> bool **)

let zle =
  coq_Z_le_gt_dec

(** val align : coq_Z -> coq_Z -> coq_Z **)

let align n amount =
  Z.mul (Z.div (Z.sub (Z.add n amount) (Zpos Coq_xH)) amount) amount

(** val floor : coq_Z -> coq_Z -> coq_Z **)

let floor n amount =
  Z.mul (Z.div n amount) amount

(** val option_eq :
    ('a1 -> 'a1 -> bool) -> 'a1 option -> 'a1 option -> bool **)

let option_eq eqA x y =
  match x with
  | Some a -> (match y with
               | Some a0 -> eqA a a0
               | None -> false)
  | None -> (match y with
             | Some _ -> false
             | None -> true)

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None

(** val proj_sumbool : bool -> bool **)

let proj_sumbool = function
| true -> true
| false -> false
