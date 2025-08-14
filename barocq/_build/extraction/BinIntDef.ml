open BinNums
open BinPos
open Datatypes

module Z =
 struct
  (** val to_nat : coq_Z -> nat **)

  let to_nat = function
  | Zpos p -> Pos.to_nat p
  | _ -> O
 end
