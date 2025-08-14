Require Import Coq.Lists.List.
Require Import Coq.Arith.Arith.
Require Import Coq.Bool.Bool.
Require Import Coq.Init.Nat.
Import ListNotations.
Require Import Pmp.

(* Hypothèses : les définitions pmp_mode, permission, pmp_register, pmp_entry, pmp_config, mem_addr
   existent déjà comme dans ton fichier. *)

(* get_bit n i = bit i (LSB = 0) of n as bool *)
Definition get_bit (n i: nat) : bool :=
  Nat.eqb (Nat.modulo (Nat.div n (Nat.pow 2 i)) 2) 1.

(* extrait l'octet index (0..3) d'un mot w (little-endian) *)
Definition byte_of_word (w byte_idx: nat) : nat :=
  Nat.modulo (Nat.div w (Nat.pow 256 byte_idx)) 256.

(* récupère l'octet de configuration d'index `idx` parmi la liste de mots cfg_words.
   idx = 0..7 (chaque mot contient 4 octets, little-endian). *)
Definition cfg_byte_at (cfg_words: list nat) (idx: nat) : nat :=
  let word_idx := Nat.div idx 4 in
  let byte_idx := Nat.modulo idx 4 in
  let w := nth word_idx cfg_words 0 in
  byte_of_word w byte_idx.


(* version fuel : on coupe après [fuel] pas -> garantit terminaison *)
Fixpoint count_trailing_ones_fuel (n fuel: nat) : nat :=
  match fuel with
  | 0 => 0
  | S fuel' =>
      if Nat.eqb (Nat.modulo n 2) 1
      then S (count_trailing_ones_fuel (Nat.div n 2) fuel')
      else 0
  end.

(* wrapper : choisir un fuel suffisant pour votre architecture.
   Pour RV32, 32 est un choix raisonnable (au plus 32 bits à tester). *)
Definition count_trailing_ones (n: nat) : nat :=
  count_trailing_ones_fuel n 32.
(* décode le champ A (bits 3..4) d'un octet *)
Definition a_bits_of_byte (b: nat) : nat :=
  Nat.modulo (Nat.div b (Nat.pow 2 3)) (Nat.pow 2 2).  (* (b >> 3) & 0x3 *)

(* décode le mode à partir de l'octet et de la valeur pmpaddr associée *)
Definition decode_mode_from (b addr_val: nat) : pmp_mode :=
  match a_bits_of_byte b with
  | 0 => OFF
  | 1 => TOR
  | 2 => NA4
  | _ => NAPOT (count_trailing_ones addr_val)
  end.

(* construction récursive des 8 entrées PMP, récursion sur [remaining] *)
Fixpoint build_entries (cfg_words addr_words: list nat) (remaining idx: nat) : list pmp_entry :=
  match remaining with
  | 0 => []
  | S remaining' =>
      let b := cfg_byte_at cfg_words idx in
      let a := nth idx addr_words 0 in
      let reg := mk_pmp_reg
                   (decode_mode_from b a)
                   (get_bit b 0)  (* R *)
                   (get_bit b 1)  (* W *)
                   (get_bit b 2)  (* X *)
      in
      (reg, a) :: build_entries cfg_words addr_words remaining' (S idx)
  end.

(* fonction principale : construire 8 entrées en partant de l'indice 0 *)
Definition array_int_to_pmp_config (cfg_words addr_words: list nat) : pmp_config :=
  build_entries cfg_words addr_words 8 0.

 Eval compute in
  array_int_to_pmp_config
    [1776413; 0] (* pmpcfg words *)
    [134316031; 536873983; 67128319; 0; 0; 0; 0; 0]. (* pmpaddr words *) 