From Verimz Require Import Int Utils.
Require Import Bool Arith.

(** * Memory model for the execution of the zones. *)

Definition mem_val := byte.

(** The memory is a list of bytes. *)
Definition mem := list mem_val.

Definition mem_addr := nat.

(** [valid_access m addr s] checks that the access at address [addr] with size [s] is in bounds of memory [m]. *)
Definition valid_access (m: mem) (addr: mem_addr) (s: nat) : Prop :=
  addr < (length m) /\
  0 < s /\
  addr + s < (length m).

Lemma valid_access_dec :
  forall m addr s,
  { valid_access m addr s } + { ~ valid_access m addr s }.
Proof.
  intros. unfold valid_access.
  destruct (lt_dec addr (length m));
  destruct (lt_dec 0 s);
  destruct (lt_dec (addr + s) (length m));
  tauto.
Defined.

Fixpoint read_aux (m: mem) (addr: mem_addr) (s : nat) : option (list mem_val) :=
  match addr, s, m with
  | O, S s', b :: m' =>
      do lv <- read_aux m' O s';
      Some (b :: lv)
  | O, O, _ => Some []
  | S addr', _, b :: m' => read_aux m' addr' s
  | _, _ , _ => None
  end.

(** [read m addr s] reads [s] values from memory [m] starting at address [addr]. *)
Definition read (m: mem) (addr: mem_addr) (s: nat) : option (list mem_val) :=
  if valid_access_dec m addr s then
    read_aux m addr s
  else None.

Fixpoint write_aux (m: mem) (addr: mem_addr) (lv: list mem_val) : option mem :=
  match addr, m, lv with
  | O, b :: m', v :: lv' =>
      do r <- write_aux m' O lv';
      Some (v :: r)
  | O, _, [] => Some m
  | S addr', b :: m', _ =>
      do r <- write_aux m' addr' lv;
      Some (b :: r)
  | _, _, _ => None
  end.

(** [write m addr lv] writes the list of values [lv] in memory [m] starting at address [addr]. *)
Definition write (m: mem) (addr: mem_addr) (lv: list mem_val) : option mem :=
  if valid_access_dec m addr (length lv) then
    write_aux m addr lv
  else None.

(** [access_overlap addr s addr' s'] checks if accesses ([addr], [s]) and ([addr'], [s']) overlap. *)
Definition access_overlap (addr: mem_addr) (s: nat) (addr': mem_addr) (s': nat): Prop :=
  addr' <= (addr + (s - 1)) /\ addr <= (addr' + (s' - 1)).

Lemma access_overlap_dec :
  forall addr s addr' s',
  { access_overlap addr s addr' s' } + { ~ access_overlap addr s addr' s' }.
Proof.
  intros. unfold access_overlap.
  destruct (le_dec addr' (addr + (s - 1)));
  destruct (le_dec addr (addr' + (s' - 1)));
  tauto.
Defined.

(** [access_contained addr s addr' s'] checks if memory access ([addr], [s]) is contained into ([addr'], [s']). *)
Definition access_contained (addr: mem_addr) (s: nat) (addr': mem_addr) (s': nat) : bool :=
  (addr' <=? addr) && (addr + s <=? addr' + s').

Section Facts.

  (* Read =============== *)

  Axiom read_valid_access :
    forall m addr s lv,
    read m addr s = Some lv ->
    valid_access m addr s.

  Axiom valid_access_read :
    forall m addr s,
    valid_access m addr s ->
    exists lv, read m addr s = Some lv.

  Axiom read_size :
    forall m addr s lv,
    read m addr s = Some lv ->
    length lv = s.
  
  (* Write ============== *)

  Axiom write_valid_access :
    forall m m' addr lv,
    write m addr lv = Some m' ->
    valid_access m addr (length lv).

  Axiom valid_access_write :
    forall m addr lv,
    valid_access m addr (length lv) ->
    exists m', write m addr lv = Some m'.
    
  Axiom write_read_same :
    forall m addr lv m',
    write m addr lv = Some m' ->
    read m' addr (length lv) = Some lv.

  Axiom write_read_diff :
    forall m m' addr addr' s lv lv',
    read m addr s = Some lv ->
    write m addr' lv' = Some m' ->
    ~ access_overlap addr s addr' (length lv') ->
    read m' addr s = Some lv.

End Facts.