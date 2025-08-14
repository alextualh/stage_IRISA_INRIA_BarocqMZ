open BinInt
open BinNums
open BinPos
open Bool
open Datatypes
open Round
open SpecFloat

type binary_float =
| B754_zero of bool
| B754_infinity of bool
| B754_nan
| B754_finite of bool * positive * coq_Z

(** val coq_SF2B : coq_Z -> coq_Z -> spec_float -> binary_float **)

let coq_SF2B _ _ = function
| S754_zero s -> B754_zero s
| S754_infinity s -> B754_infinity s
| S754_nan -> B754_nan
| S754_finite (s, m, e) -> B754_finite (s, m, e)

(** val coq_B2SF : coq_Z -> coq_Z -> binary_float -> spec_float **)

let coq_B2SF _ _ = function
| B754_zero s -> S754_zero s
| B754_infinity s -> S754_infinity s
| B754_nan -> S754_nan
| B754_finite (s, m, e) -> S754_finite (s, m, e)

(** val coq_Bcompare :
    coq_Z -> coq_Z -> binary_float -> binary_float -> comparison option **)

let coq_Bcompare prec emax f1 f2 =
  coq_SFcompare (coq_B2SF prec emax f1) (coq_B2SF prec emax f2)

type mode =
| Coq_mode_NE
| Coq_mode_ZR
| Coq_mode_DN
| Coq_mode_UP
| Coq_mode_NA

(** val choice_mode : mode -> bool -> coq_Z -> location -> coq_Z **)

let choice_mode m sx mx lx =
  match m with
  | Coq_mode_NE -> cond_incr (round_N (negb (Z.even mx)) lx) mx
  | Coq_mode_ZR -> mx
  | Coq_mode_DN -> cond_incr (round_sign_DN sx lx) mx
  | Coq_mode_UP -> cond_incr (round_sign_UP sx lx) mx
  | Coq_mode_NA -> cond_incr (round_N true lx) mx

(** val overflow_to_inf : mode -> bool -> bool **)

let overflow_to_inf m s =
  match m with
  | Coq_mode_ZR -> false
  | Coq_mode_DN -> s
  | Coq_mode_UP -> negb s
  | _ -> true

(** val binary_overflow : coq_Z -> coq_Z -> mode -> bool -> spec_float **)

let binary_overflow prec emax m s =
  if overflow_to_inf m s
  then S754_infinity s
  else S754_finite (s,
         (Z.to_pos (Z.sub (Z.pow (Zpos (Coq_xO Coq_xH)) prec) (Zpos Coq_xH))),
         (Z.sub emax prec))

(** val binary_fit_aux :
    coq_Z -> coq_Z -> mode -> bool -> positive -> coq_Z -> spec_float **)

let binary_fit_aux prec emax mode0 sx mx ex =
  if Z.leb ex (Z.sub emax prec)
  then S754_finite (sx, mx, ex)
  else binary_overflow prec emax mode0 sx

(** val binary_round_aux :
    coq_Z -> coq_Z -> mode -> bool -> coq_Z -> coq_Z -> location -> spec_float **)

let binary_round_aux prec emax mode0 sx mx ex lx =
  let (mrs', e') = shr_fexp prec emax mx ex lx in
  let (mrs'', e'') =
    shr_fexp prec emax
      (choice_mode mode0 sx mrs'.shr_m (loc_of_shr_record mrs')) e'
      Coq_loc_Exact
  in
  (match mrs''.shr_m with
   | Z0 -> S754_zero sx
   | Zpos m -> binary_fit_aux prec emax mode0 sx m e''
   | Zneg _ -> S754_nan)

(** val coq_Bmult :
    coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float **)

let coq_Bmult prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_zero sy -> B754_zero (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
     | _ -> B754_nan)
  | B754_infinity sx ->
    (match y with
     | B754_infinity sy -> B754_infinity (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
     | _ -> B754_nan)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero sy -> B754_zero (xorb sx sy)
     | B754_infinity sy -> B754_infinity (xorb sx sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       coq_SF2B prec emax
         (binary_round_aux prec emax m (xorb sx sy) (Zpos (Pos.mul mx my))
           (Z.add ex ey) Coq_loc_Exact))

(** val shl_align_fexp :
    coq_Z -> coq_Z -> positive -> coq_Z -> positive * coq_Z **)

let shl_align_fexp prec emax mx ex =
  shl_align mx ex (fexp prec emax (Z.add (Zpos (digits2_pos mx)) ex))

(** val binary_round :
    coq_Z -> coq_Z -> mode -> bool -> positive -> coq_Z -> spec_float **)

let binary_round prec emax m sx mx ex =
  let (mz, ez) = shl_align_fexp prec emax mx ex in
  binary_round_aux prec emax m sx (Zpos mz) ez Coq_loc_Exact

(** val binary_normalize :
    coq_Z -> coq_Z -> mode -> coq_Z -> coq_Z -> bool -> binary_float **)

let binary_normalize prec emax mode0 m e szero =
  match m with
  | Z0 -> B754_zero szero
  | Zpos m0 -> coq_SF2B prec emax (binary_round prec emax mode0 false m0 e)
  | Zneg m0 -> coq_SF2B prec emax (binary_round prec emax mode0 true m0 e)

(** val coq_Fplus_naive :
    bool -> positive -> coq_Z -> bool -> positive -> coq_Z -> coq_Z -> coq_Z **)

let coq_Fplus_naive sx mx ex sy my ey ez =
  Z.add (cond_Zopp sx (Zpos (fst (shl_align mx ex ez))))
    (cond_Zopp sy (Zpos (fst (shl_align my ey ez))))

(** val coq_Bplus :
    coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float **)

let coq_Bplus prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_zero sy ->
       if eqb sx sy
       then x
       else (match m with
             | Coq_mode_DN -> B754_zero true
             | _ -> B754_zero false)
     | B754_nan -> B754_nan
     | _ -> y)
  | B754_infinity sx ->
    (match y with
     | B754_infinity sy -> if eqb sx sy then x else B754_nan
     | B754_nan -> B754_nan
     | _ -> x)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero _ -> x
     | B754_infinity _ -> y
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       let ez = Z.min ex ey in
       binary_normalize prec emax m (coq_Fplus_naive sx mx ex sy my ey ez) ez
         (match m with
          | Coq_mode_DN -> true
          | _ -> false))

(** val coq_Bminus :
    coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float **)

let coq_Bminus prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_zero sy ->
       if eqb sx (negb sy)
       then x
       else (match m with
             | Coq_mode_DN -> B754_zero true
             | _ -> B754_zero false)
     | B754_infinity sy -> B754_infinity (negb sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) -> B754_finite ((negb sy), my, ey))
  | B754_infinity sx ->
    (match y with
     | B754_infinity sy -> if eqb sx (negb sy) then x else B754_nan
     | B754_nan -> B754_nan
     | _ -> x)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero _ -> x
     | B754_infinity sy -> B754_infinity (negb sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       let ez = Z.min ex ey in
       binary_normalize prec emax m
         (coq_Fplus_naive sx mx ex (negb sy) my ey ez) ez
         (match m with
          | Coq_mode_DN -> true
          | _ -> false))

(** val coq_Bdiv :
    coq_Z -> coq_Z -> mode -> binary_float -> binary_float -> binary_float **)

let coq_Bdiv prec emax m x y =
  match x with
  | B754_zero sx ->
    (match y with
     | B754_infinity sy -> B754_zero (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_zero (xorb sx sy)
     | _ -> B754_nan)
  | B754_infinity sx ->
    (match y with
     | B754_zero sy -> B754_infinity (xorb sx sy)
     | B754_finite (sy, _, _) -> B754_infinity (xorb sx sy)
     | _ -> B754_nan)
  | B754_nan -> B754_nan
  | B754_finite (sx, mx, ex) ->
    (match y with
     | B754_zero sy -> B754_infinity (xorb sx sy)
     | B754_infinity sy -> B754_zero (xorb sx sy)
     | B754_nan -> B754_nan
     | B754_finite (sy, my, ey) ->
       coq_SF2B prec emax
         (let (p, lz) =
            coq_SFdiv_core_binary prec emax (Zpos mx) ex (Zpos my) ey
          in
          let (mz, ez) = p in
          binary_round_aux prec emax m (xorb sx sy) mz ez lz))
