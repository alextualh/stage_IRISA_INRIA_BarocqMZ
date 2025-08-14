Require Extraction.
Require ExtrOcamlBasic.
Require ExtrOcamlZInt.
Require ExtrOcamlNatInt.

Extraction Language OCaml.
(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or  (at your option) any later version.               *)
(*  This file is also distributed under the terms of the               *)
(*  INRIA Non-Commercial License Agreement.                            *)
(*                                                                     *)
(* *********************************************************************)

(* Additional extraction directives specific to the RISC-V port *)

From compcert Require Archi Asm.
From compcert Require Import Integers.

(* Archi *)

Extract Constant Archi.ptr64 => "false".
Extract Constant Archi.pic_code => "fun () -> false".  (* for the time being *)

(* Asm *)
Extract Constant Asm.low_half => "fun _ _ _ -> assert false".
Extract Constant Asm.high_half => "fun _ _ _ -> assert false".
(* ________________________________________________________________________ *)

From Verimz Require Memory Kernel Utils Barocqmzutils.
From compcert Require Import Maps.
Require Import ZArith.

Extraction Blacklist List Int.

Extract Constant Barocqmz.Br_interface_QUANTUM_CYC => "100".

Extract Inductive Z => "Stdlib.Int.t" [ "0" "" "(~-)" ]
"(fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))".

Separate Extraction
    Kernel.step
    Kernel.mie_machine_software
    Utils.make
    PTree.set
    Barocqmzutils.