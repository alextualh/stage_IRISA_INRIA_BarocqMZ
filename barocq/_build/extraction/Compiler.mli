open Barocq
open BarocqBNFgen
open ClightCe
open ClightCegen
open Errors
open Imp1
open Imp1gen
open Imp2gen
open ImpABNFgen
open ImpBNFgen
open Monads
open Target

val compile : bool -> archi -> Barocq.program -> ClightCe.program res

val compile_to_imp1 : archi -> Barocq.program -> program res

val aliascheck_program :
  bool -> archi -> Barocq.program -> Imp1Typed.program res
