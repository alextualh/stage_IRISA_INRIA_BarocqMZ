From Verimz Require Import Utils Memory.
Require Import Bool Nat.

(** * Modelisation of the RISC-V Physical Memory Protection (PMP) mecanism. *)

Inductive pmp_mode :=
  | OFF : pmp_mode
  | TOR : pmp_mode
  | NA4 : pmp_mode
  | NAPOT : nat -> pmp_mode.

Inductive permission :=
  | Readable
  | Writable
  | Executable.

Record pmp_register := mk_pmp_reg {
  mode: pmp_mode;
  perm_read: bool;
  perm_write: bool;
  perm_exec: bool;
}.

Definition pmp_entry : Set := pmp_register * mem_addr.

(** A pmp configuration is a list of register pairs (one configuration register and one address register). *)
Definition pmp_config := list pmp_entry.

(** A pmp region is represented by its base memory address and its size. *)
Definition pmp_region : Set := mem_addr * nat.

(** [pmp_entry_to_region entry prev_entry] interprets a pmp entry into a pmp region.
    The pmp mode TOR requires the previous pmp entry for the region range to be determined. *)
Definition pmp_entry_to_region (entry: pmp_entry) (prev_entry: option pmp_entry) : option pmp_region :=
  let (reg, addr) := entry in
  match (mode reg) with
  | OFF => None
  | TOR => 
      let bottom := match prev_entry with
      | None => 0
      | Some (_, paddr) => paddr
      end in
      Some (bottom, (addr - bottom) + 1)
  | NA4 => Some (addr, 4)
  | NAPOT s => Some (addr, pow 2 s)
  end.

(** [pmp_permission_check entry perm] checks if pmp entry [entry] has permission [perm]. *)
Definition pmp_permission_check (entry: pmp_entry) (perm: permission) : bool :=
  let (r, _) := entry in
  match perm with
  | Readable => perm_read r
  | Writable => perm_write r
  | Executable => perm_exec r
  end.

Fixpoint pmp_access_check_rec (cfg: pmp_config) (m: mem) (addr: mem_addr) (s: nat) (perm: permission) (pe: option pmp_entry) : bool :=
  match cfg with
  | [] => false
  | e :: cfg' =>
      match (pmp_entry_to_region e pe) with
      | None => pmp_access_check_rec cfg' m addr s perm (Some e)
      | Some (er, sr) =>
          if access_overlap_dec addr s er sr then
            (pmp_permission_check e perm) && (access_contained addr s er sr)
          else pmp_access_check_rec cfg' m addr s perm (Some e)
      end
  end.

(** [pmp_access_check_b cfg m addr s perm] checks if the memory access (addr, s, perm) is valid for memory [m] given the pmp config [cfg].*)
Definition pmp_access_check_b (cfg: pmp_config) (m: mem) (addr: mem_addr) (s: nat) (perm: permission) : bool :=
  pmp_access_check_rec cfg m addr s perm None.

(** [pmp_acess_check_ cfg m addr s perm] is the logical version of [pmp_access_check_b].*)
Definition pmp_access_check (cfg: pmp_config) (m: mem) (addr: mem_addr) (s: nat) (perm: permission) : Prop :=
  pmp_access_check_b cfg m addr s perm = true.

(** [valid_pmp_entry entry prev_entry] checks if the bottom of a pmp entry is strictly smaller than the of the region. *)
Definition valid_pmp_entry (entry: pmp_entry) (prev_entry: option pmp_entry) : bool :=
  match (pmp_entry_to_region entry prev_entry) with
  | Some (bottom, top) => bottom <? top
  | None => true
  end.

(** [valid_pmp_config cfg] checks if the pmp config [cfg] is valid. *)
Definition valid_pmp_config (cfg: pmp_config) : bool :=
  fst (fold_left (fun '(b, x) e => (b && (valid_pmp_entry e x), Some e)) cfg (true, None)).

Inductive pmp_access :=
  | MLoad : mem_addr -> nat -> pmp_access
  | MStore : mem_addr -> list (mem_val) -> pmp_access
  | MFetch : mem_addr -> nat -> pmp_access.

(** [pmp_load cfg m addr s] loads [s] values from memory [m] given the pmp config [cfg], starting at address [addr]. *)
Definition pmp_load (cfg: pmp_config) (m: mem) (addr: mem_addr) (s: nat) : option (list mem_val) :=
  if pmp_access_check_b cfg m addr s Readable then
    read m addr s
  else None.

(** [pmp_store cfg m addr lv] stores the list of values [lv] in memory [m] given the pmp config [cfg], starting at address [addr]. *)
Definition pmp_store (cfg: pmp_config) (m: mem) (addr: mem_addr) (lv: list mem_val) : option mem :=
  if pmp_access_check_b cfg m addr (length lv) Writable then
    write m addr lv
  else None.

(** [pmp_fetch] fetches instructions (size [s]) from memory [m] given the pmp config [cfg], starting at address [addr]. *)
Definition pmp_fetch (cfg: pmp_config) (m: mem) (addr: mem_addr) (s: nat) : option (list mem_val) :=
  if pmp_access_check_b cfg m addr s Executable then
    read m addr s
  else None.

(** [exec_pmp_access cfg m pmpa] executes the pmp access [pmpa] and returns the new memory. *)
Definition exec_pmp_access (cfg: pmp_config) (m: mem) (pmpa: pmp_access) : option mem :=
  match pmpa with
  | MLoad addr s =>
      do lv <- pmp_load cfg m addr s;
      Some m
  | MStore addr lv =>
      do m' <- pmp_store cfg m addr lv;
      Some m'
  | MFetch addr s =>
      do lv <- pmp_fetch cfg m addr s;
      Some m
  end.
  
Section Facts.

  (* Load =============== *)

  Axiom pmp_access_check_imp_valid_access :
    forall cfg m addr s perm,
    pmp_access_check cfg m addr s perm -> valid_access m addr s.

  Axiom pmp_load_read :
    forall cfg m addr s lv,
    pmp_load cfg m addr s = Some lv ->
    read m addr s = Some lv.

  Axiom read_pmp_load :
    forall cfg m addr s lv,
    read m addr s = Some lv ->
    pmp_access_check cfg m addr s Readable ->
    pmp_load cfg m addr s = Some lv.
    
  Axiom pmp_load_access_check :
    forall cfg m addr s lv,
    pmp_load cfg m addr s = Some lv ->
    pmp_access_check cfg m addr s Readable.

  Axiom pmp_access_check_load :
    forall cfg m addr s,
    pmp_access_check cfg m addr s Readable ->
    exists lv, pmp_load cfg m addr s = Some lv.

  Axiom pmp_load_size :
    forall cfg m addr s lv,
    pmp_load cfg m addr s = Some lv ->
    length lv = s.

  (* Write ============== *)

  Axiom pmp_store_write :
    forall cfg m addr lv m',
    pmp_store cfg m addr lv = Some m' ->
    write m addr lv = Some m'.

  Axiom write_store_pmp :
    forall cfg m addr lv m',
    write m addr lv = Some m' ->
    pmp_access_check cfg m addr (length lv) Writable ->
    pmp_store cfg m addr lv = Some m'.

  Axiom pmp_store_access_check :
    forall cfg m m' addr lv,
    pmp_store cfg m addr lv = Some m' ->
    pmp_access_check cfg m addr (length lv) Writable.

  Axiom pmp_access_check_store :
    forall cfg m addr lv,
    pmp_access_check cfg m addr (length lv) Writable ->
    exists m', pmp_store cfg m addr lv = Some m'.

  Axiom pmp_store_read_same :
    forall cfg m m' addr lv,
    pmp_store cfg m addr lv = Some m' ->
    read m' addr (length lv) = Some lv.

  Axiom pmp_store_read_diff :
    forall cfg m m' addr addr' s lv lv',
    read m addr s = Some lv ->
    pmp_store cfg m addr' lv' = Some m' ->
    ~ access_overlap addr s addr' (length lv') ->
    read m' addr s = Some lv.

  (* Fetch ============== *)

  Axiom pmp_fetch_read :
    forall cfg m addr s lv,
    pmp_fetch cfg m addr s = Some lv ->
    read m addr s = Some lv.

  Axiom read_pmp_fetch :
    forall cfg m addr s lv,
    read m addr s = Some lv ->
    pmp_access_check cfg m addr s Executable ->
    pmp_fetch cfg m addr s = Some lv.
    
  Axiom pmp_fetch_access_check :
    forall cfg m addr s lv,
    pmp_fetch cfg m addr s = Some lv ->
    pmp_access_check cfg m addr s Executable.

  Axiom pmp_access_check_fetch :
    forall cfg m addr s,
    pmp_access_check cfg m addr s Executable ->
    exists lv, pmp_fetch cfg m addr s = Some lv.

  Axiom pmp_fetch_size :
    forall cfg m addr s lv,
    pmp_fetch cfg m addr s = Some lv ->
    length lv = s.

End Facts.

(* ___________ ADD ALEX CONVERT PMP_CONFIG to ARRAY INT as used in zone struct field pmp *)

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
