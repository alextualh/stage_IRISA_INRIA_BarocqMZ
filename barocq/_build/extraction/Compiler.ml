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

(** val compile : bool -> archi -> Barocq.program -> ClightCe.program res **)

let compile show_debug arch prog =
  MonError.bind (BarocqBNFgen.norm_program arch prog) (fun bbnf ->
    let ibnf = transl_program bbnf in
    MonError.bind (norm_program ibnf) (fun iabnf ->
      let imp1 = Imp1gen.transl_program iabnf in
      MonError.bind (Imp1Typing.typecheck_program arch imp1)
        (fun imp1_typed ->
        MonError.bind (gen_aliasing_program show_debug imp1_typed)
          (fun imp1_alias ->
          MonError.bind (check_program_aliasing imp1_alias)
            (fun imp1_typed0 ->
            let imp2 = Imp2gen.transl_program imp1_typed0 in
            MonError.bind (ClightCegen.transl_program imp2) MonError.ret)))))

(** val compile_to_imp1 : archi -> Barocq.program -> program res **)

let compile_to_imp1 arch prog =
  MonError.bind (BarocqBNFgen.norm_program arch prog) (fun bbnf ->
    let ibnf = transl_program bbnf in
    MonError.bind (norm_program ibnf) (fun iabnf ->
      let imp1 = Imp1gen.transl_program iabnf in MonError.ret imp1))

(** val aliascheck_program :
    bool -> archi -> Barocq.program -> Imp1Typed.program res **)

let aliascheck_program show_debug arch prog =
  MonError.bind (compile_to_imp1 arch prog) (fun imp1 ->
    MonError.bind (Imp1Typing.typecheck_program arch imp1) (fun imp1_typed ->
      MonError.bind (gen_aliasing_program show_debug imp1_typed)
        check_program_aliasing))
