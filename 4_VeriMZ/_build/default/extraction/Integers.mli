open BinInt
open Coqlib
open Datatypes
open List0
open Zbits
open Zpower

type comparison =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module type WORDSIZE =
 sig
  val wordsize : int
 end

module Make :
 functor (WS:WORDSIZE) ->
 sig
  val wordsize : int

  val zwordsize : Stdlib.Int.t

  val modulus : Stdlib.Int.t

  val half_modulus : Stdlib.Int.t

  val max_unsigned : Stdlib.Int.t

  val max_signed : Stdlib.Int.t

  val min_signed : Stdlib.Int.t

  type int =
    Stdlib.Int.t
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> Stdlib.Int.t

  val coq_Z_mod_modulus : Stdlib.Int.t -> Stdlib.Int.t

  val unsigned : int -> Stdlib.Int.t

  val signed : int -> Stdlib.Int.t

  val repr : Stdlib.Int.t -> int

  val zero : int

  val one : int

  val mone : int

  val iwordsize : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divs : int -> int -> int

  val mods : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val not : int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val shr : int -> int -> int

  val rol : int -> int -> int

  val ror : int -> int -> int

  val rolm : int -> int -> int -> int

  val shrx : int -> int -> int

  val mulhu : int -> int -> int

  val mulhs : int -> int -> int

  val negative : int -> int

  val add_carry : int -> int -> int -> int

  val add_overflow : int -> int -> int -> int

  val sub_borrow : int -> int -> int -> int

  val sub_overflow : int -> int -> int -> int

  val shr_carry : int -> int -> int

  val zero_ext : Stdlib.Int.t -> int -> int

  val sign_ext : Stdlib.Int.t -> int -> int

  val one_bits : int -> int list

  val is_power2 : int -> int option

  val cmp : comparison -> int -> int -> bool

  val cmpu : comparison -> int -> int -> bool

  val notbool : int -> int

  val divmodu2 : int -> int -> int -> (int * int) option

  val divmods2 : int -> int -> int -> (int * int) option

  val testbit : int -> Stdlib.Int.t -> bool

  val int_of_one_bits : int list -> int

  val no_overlap : int -> Stdlib.Int.t -> int -> Stdlib.Int.t -> bool

  val size : int -> Stdlib.Int.t

  val unsigned_bitfield_extract : Stdlib.Int.t -> Stdlib.Int.t -> int -> int

  val signed_bitfield_extract : Stdlib.Int.t -> Stdlib.Int.t -> int -> int

  val bitfield_insert : Stdlib.Int.t -> Stdlib.Int.t -> int -> int -> int
 end

module Wordsize_32 :
 sig
  val wordsize : int
 end

module Int :
 sig
  val wordsize : int

  val zwordsize : Stdlib.Int.t

  val modulus : Stdlib.Int.t

  val half_modulus : Stdlib.Int.t

  val max_unsigned : Stdlib.Int.t

  val max_signed : Stdlib.Int.t

  val min_signed : Stdlib.Int.t

  type int =
    Stdlib.Int.t
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> Stdlib.Int.t

  val coq_Z_mod_modulus : Stdlib.Int.t -> Stdlib.Int.t

  val unsigned : int -> Stdlib.Int.t

  val signed : int -> Stdlib.Int.t

  val repr : Stdlib.Int.t -> int

  val zero : int

  val one : int

  val mone : int

  val iwordsize : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divs : int -> int -> int

  val mods : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val not : int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val shr : int -> int -> int

  val rol : int -> int -> int

  val ror : int -> int -> int

  val rolm : int -> int -> int -> int

  val shrx : int -> int -> int

  val mulhu : int -> int -> int

  val mulhs : int -> int -> int

  val negative : int -> int

  val add_carry : int -> int -> int -> int

  val add_overflow : int -> int -> int -> int

  val sub_borrow : int -> int -> int -> int

  val sub_overflow : int -> int -> int -> int

  val shr_carry : int -> int -> int

  val zero_ext : Stdlib.Int.t -> int -> int

  val sign_ext : Stdlib.Int.t -> int -> int

  val one_bits : int -> int list

  val is_power2 : int -> int option

  val cmp : comparison -> int -> int -> bool

  val cmpu : comparison -> int -> int -> bool

  val notbool : int -> int

  val divmodu2 : int -> int -> int -> (int * int) option

  val divmods2 : int -> int -> int -> (int * int) option

  val testbit : int -> Stdlib.Int.t -> bool

  val int_of_one_bits : int list -> int

  val no_overlap : int -> Stdlib.Int.t -> int -> Stdlib.Int.t -> bool

  val size : int -> Stdlib.Int.t

  val unsigned_bitfield_extract : Stdlib.Int.t -> Stdlib.Int.t -> int -> int

  val signed_bitfield_extract : Stdlib.Int.t -> Stdlib.Int.t -> int -> int

  val bitfield_insert : Stdlib.Int.t -> Stdlib.Int.t -> int -> int -> int
 end

module Wordsize_8 :
 sig
  val wordsize : int
 end

module Byte :
 sig
  val wordsize : int

  val zwordsize : Stdlib.Int.t

  val modulus : Stdlib.Int.t

  val half_modulus : Stdlib.Int.t

  val max_unsigned : Stdlib.Int.t

  val max_signed : Stdlib.Int.t

  val min_signed : Stdlib.Int.t

  type int =
    Stdlib.Int.t
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> Stdlib.Int.t

  val coq_Z_mod_modulus : Stdlib.Int.t -> Stdlib.Int.t

  val unsigned : int -> Stdlib.Int.t

  val signed : int -> Stdlib.Int.t

  val repr : Stdlib.Int.t -> int

  val zero : int

  val one : int

  val mone : int

  val iwordsize : int

  val eq_dec : int -> int -> bool

  val eq : int -> int -> bool

  val lt : int -> int -> bool

  val ltu : int -> int -> bool

  val neg : int -> int

  val add : int -> int -> int

  val sub : int -> int -> int

  val mul : int -> int -> int

  val divs : int -> int -> int

  val mods : int -> int -> int

  val divu : int -> int -> int

  val modu : int -> int -> int

  val coq_and : int -> int -> int

  val coq_or : int -> int -> int

  val xor : int -> int -> int

  val not : int -> int

  val shl : int -> int -> int

  val shru : int -> int -> int

  val shr : int -> int -> int

  val rol : int -> int -> int

  val ror : int -> int -> int

  val rolm : int -> int -> int -> int

  val shrx : int -> int -> int

  val mulhu : int -> int -> int

  val mulhs : int -> int -> int

  val negative : int -> int

  val add_carry : int -> int -> int -> int

  val add_overflow : int -> int -> int -> int

  val sub_borrow : int -> int -> int -> int

  val sub_overflow : int -> int -> int -> int

  val shr_carry : int -> int -> int

  val zero_ext : Stdlib.Int.t -> int -> int

  val sign_ext : Stdlib.Int.t -> int -> int

  val one_bits : int -> int list

  val is_power2 : int -> int option

  val cmp : comparison -> int -> int -> bool

  val cmpu : comparison -> int -> int -> bool

  val notbool : int -> int

  val divmodu2 : int -> int -> int -> (int * int) option

  val divmods2 : int -> int -> int -> (int * int) option

  val testbit : int -> Stdlib.Int.t -> bool

  val int_of_one_bits : int list -> int

  val no_overlap : int -> Stdlib.Int.t -> int -> Stdlib.Int.t -> bool

  val size : int -> Stdlib.Int.t

  val unsigned_bitfield_extract : Stdlib.Int.t -> Stdlib.Int.t -> int -> int

  val signed_bitfield_extract : Stdlib.Int.t -> Stdlib.Int.t -> int -> int

  val bitfield_insert : Stdlib.Int.t -> Stdlib.Int.t -> int -> int -> int
 end

module Wordsize_64 :
 sig
  val wordsize : int
 end

module Int64 :
 sig
  val wordsize : int

  val modulus : Stdlib.Int.t

  type int =
    Stdlib.Int.t
    (* singleton inductive, whose constructor was mkint *)

  val intval : int -> Stdlib.Int.t

  val coq_Z_mod_modulus : Stdlib.Int.t -> Stdlib.Int.t

  val unsigned : int -> Stdlib.Int.t

  val repr : Stdlib.Int.t -> int

  val zero : int

  val eq : int -> int -> bool

  val ltu : int -> int -> bool

  val add : int -> int -> int

  val sub : int -> int -> int

  val cmpu : comparison -> int -> int -> bool
 end
