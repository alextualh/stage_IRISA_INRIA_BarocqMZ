From Verimz Require Import Int.

(** * Representation of a RISC-V machine. *) 

(** A machine is a collection of registers. *)
Record machine := mk_machine {
  mtime: int64;     (* machine timer *)
  mtimecmp: int64;  (* machine timer compare register *)
  mie: intptr       (* interrupts enabled register *) 
}.

Definition read_mtime := mtime.

Definition read_mtimecmp := mtimecmp.

Definition write_mtime (mc: machine) (v: int64) : machine :=
  mk_machine v (mtimecmp mc) (mie mc).

Definition write_mtimecmp (mc: machine) (v: int64) : machine :=
  mk_machine (mtime mc) v (mie mc).

Definition read_mie := mie.

Definition write_mie (mc: machine) (v: intptr) : machine :=
  mk_machine (mtime mc) (mtimecmp mc) v.

Definition set_mie (mc: machine) (ofs: intptr) : machine :=
  let mie' := Intptr.or (mie mc) (Intptr.shl Intptr.one ofs) in
  mk_machine (mtime mc) (mtimecmp mc) (mie mc).
