From Verimz Require Import Int Memory Pmp Barocqmzutils.
From compcert Require Import Archi Maps Memdata Integers.
Require Import ZArith PArith.
From BarocqComp Require Import Error Barray Intop.

    (* From Kernel.v to avoid cylce dep with BarocMZ utils and used after *)

  Inductive op_code :=
    | OpUret
    | OpOther.

    
  Inductive interrupt :=
    | IntrpMachineExternal : interrupt
    | IntrpMachineSoftware : interrupt
    | IntrpMachineTimer : interrupt.
    (** Implemented ecalls. *)
Inductive ecall :=
  | EcallExcptVect : intptr -> intptr -> ecall
  | EcallIntrpVect : intptr -> intptr -> ecall
  | EcallCsrsMie : ecall
  | EcallCsrcMie : ecall.


    (** Software exceptions are raised by an unauthorized pmp access or by the "ecall" and "uret" instructions. *)
  Inductive exception :=
    | ExcInstrAccessFault : pmp_access -> exception
    | ExcLoadAccessFault : pmp_access -> exception
    | ExcStoreAccessFault : pmp_access -> exception
    | ExcEcall : ecall -> exception
    | ExcIllegalInstr : op_code -> exception.


  Inductive trap :=
    | TrpInterrupt : interrupt -> trap
    | TrpException : exception -> trap.


  Definition interrupt_code (intrp: interrupt) : intptr :=
    let code_n := match intrp with
    | IntrpMachineSoftware => 3
    | IntrpMachineTimer => 7
    | IntrpMachineExternal => 11
    end in Intptr.of_nat code_n.

Open Scope Z_scope.

(* ______________________________________________________________________________ *)
(** SHIFT_INT_BIT pour 32 -> bit d'interruption = 31 *)
Definition interrupt_bit_shift : Z := 31%Z.
Definition interrupt_bit_mask : Z := Z.shiftl 1 interrupt_bit_shift.  (* = 2^31 *)

(** codes RISC-V pour les interruptions machine *)
Definition interrupt_code_of (i : interrupt) : nat :=
  match i with
  | IntrpMachineSoftware => 3   (* Machine software interrupt *)
  | IntrpMachineTimer    => 7   (* Machine timer interrupt *)
  | IntrpMachineExternal => 11  (* Machine external interrupt *)
  end.


(** construire un mcause (avec bit interruption à 1) pour une interruption *)
Definition mk_mcause_for_interrupt (i : interrupt) : int :=
  Int.repr (Z.add interrupt_bit_mask (Z.of_nat (interrupt_code_of i))).

Definition mtval_of_interrupt (_ : interrupt) : int := Int.zero.

Definition exception_code (exc: exception) : intptr :=
    let code_n := match exc with
    | ExcInstrAccessFault _ => 1
    | ExcIllegalInstr _ => 2
    | ExcLoadAccessFault _ => 5
    | ExcStoreAccessFault _ => 7
    | ExcEcall _ => 8
    end in
    Intptr.repr code_n.


Definition mem_addr_to_int (a : nat) : int :=
  Int.repr (Z.of_nat a).
Definition op_code_to_int_default (_ : op_code) : int := Int.zero. (* je sais pas trop comment convertir op_code, pas très grave *)
Definition mtval_of_exception (e : exception) : int :=
  match e with
  | ExcInstrAccessFault pmpa
  | ExcLoadAccessFault pmpa
  | ExcStoreAccessFault pmpa =>
      match pmpa with
      | MLoad addr _ => mem_addr_to_int addr
      | MStore addr _ => mem_addr_to_int addr
      | MFetch addr _ => mem_addr_to_int addr
      end
  | ExcIllegalInstr _opc => op_code_to_int_default _opc
  | ExcEcall _ =>
      Int.zero
  end.

 
Definition mk_mcause_for_exception (e : exception) : int :=
  Intptr_to_int (exception_code e).

(** Fonction principale *)
Definition trap_to_mcause (t : trap) : int * int :=
  match t with
  | TrpInterrupt i =>
      (mk_mcause_for_interrupt i, mtval_of_interrupt i)
  | TrpException e =>
      (mk_mcause_for_exception e, mtval_of_exception e)
  end.
(* ______________________________________________________________________________ *)

(** * Abstract syntax and semantics for a very simple RISC-V -like assembly language. *)
(** ASM programs are meant to be run in user-mode. *)


Inductive ireg :=
  | X1 : ireg
  | X2 : ireg
  | X3 : ireg.

Lemma ireg_eq :
  forall (x y: ireg), {x = y} + {x <> y}.
Proof.
  decide equality.
Defined.

Inductive preg :=
  | PC : preg
  | IR : ireg -> preg.

Coercion IR : ireg >-> preg.

Lemma preg_eq :
  forall (x y: preg), {x = y} + {x <> y}.
Proof.
  decide equality. apply ireg_eq.
Qed.

Module PregEq.
  Definition t := preg.
  Definition eq := preg_eq.
End PregEq.

Module Pregmap := EMap(PregEq).

(** Map from registers to values. *)
Definition regset := Pregmap.t intptr.

Definition label := positive.

(** Label environment that associates each label with a memory address. *)
Definition genv := PTree.t intptr.

(* * Implemented ecalls.
Inductive ecall :=
  | EcallExcptVect : intptr -> intptr -> ecall
  | EcallIntrpVect : intptr -> intptr -> ecall
  | EcallCsrsMie : ecall
  | EcallCsrcMie : ecall. *)

(** Instruction set. *)
Inductive instruction :=
  | Iadd : ireg -> ireg -> ireg -> instruction
  | Isub : ireg -> ireg -> ireg -> instruction
  | Ili : ireg -> intptr -> instruction
  | Ilb : ireg -> ireg -> intptr -> instruction
  | Isb : ireg -> ireg -> intptr -> instruction
  | Ilabel : label -> instruction
  | Ijump : label -> instruction
  | Iecall : ecall -> instruction
  | Iuret : instruction.

Definition code := list instruction.

Record program := mk_program {
  prog_code: code;
  prog_genv: genv;
}.

Declare Scope asm.

Notation "a # b" := (a b) (at level 1, only parsing) : asm.
Notation "a # b <- c" := (Pregmap.set b c a) (at level 1, b at next level) : asm.

Open Scope asm.

Section Semantics.

  (* Inductive op_code :=
    | OpUret
    | OpOther. *)

  (** Software exceptions are raised by an unauthorized pmp access or by the "ecall" and "uret" instructions. *)
  (* Inductive exception :=
    | ExcInstrAccessFault : pmp_access -> exception
    | ExcLoadAccessFault : pmp_access -> exception
    | ExcStoreAccessFault : pmp_access -> exception
    | ExcEcall : ecall -> exception
    | ExcIllegalInstr : op_code -> exception. *)

  Inductive outcome :=
    | Next : array int -> mem -> option pmp_access -> outcome
    | Exception : exception -> outcome
    | Stuck : outcome.

  Definition instr_size : nat := 4.

  Definition addr_of_intptr (iptr: intptr) := Intptr.unsigned_to_nat iptr.
  
  Print Barray.get.
    (* ADD ALEX : *)
Definition code_ireg (r : ireg) : int :=
  match r with
  | X1 => Int.repr 1
  | X2 => Int.repr 2
  | X3 => Int.repr 3
  end.
Definition code_preg (p : preg) : int64 :=
  match p with
  | PC    => Int64.repr 7
  | IR r  => Int64.repr (Int.signed (code_ireg r))
  end.
(* ADD ALEX end *)

  Definition get_reg_in_set (rs: array int) (index_preg: preg) : Intptr.int :=
  let index := (code_preg index_preg) in
  match Barray.get rs (code_preg PC) with
  | OK val_index => int_to_Intptr val_index
  | Error _ => Intptr.zero 
  end.
(* pour pas écrire : let val_PC := Barray.get rs (code_preg PC) in *)
  Print Barray.set.

  Definition set_reg_in_set (rs: array int) (index_preg: preg) (new_val: int) : array int :=
  let index := (code_preg index_preg) in
  match Barray.set rs (code_preg PC) new_val with
  | OK reg_set => reg_set
  | Error _ => rs 
  end.

  Definition nextinstr (rs: array int) : array int := (set_reg_in_set rs PC (Intptr_to_int (Intptr.add (get_reg_in_set rs PC) (Intptr.of_nat instr_size)))).
    
Print Next.
Locate Next.
Print array.
Locate int.

  Definition array_int_to_list_nat (a : array int) : list nat :=
    List.map (fun x => Z.to_nat (Int.unsigned x)) a.

    Print array_int_to_pmp_config.

    (** [pmp_exec_instr i rs ge m pmpcfg] executes instruction [i].
      The pmp configuration [pmpcfg] is used to check whether the instruction fetch
      and the potential memory access are allowed. *)
  Definition pmp_exec_instr (i: instruction) (rs: array int) (ge: genv) (m: mem) (pmpcfg_array: array int) (pmpaddr_array : array int) : outcome :=
    let pmpcfg := array_int_to_pmp_config (array_int_to_list_nat pmpcfg_array) (array_int_to_list_nat pmpaddr_array) in 
    match (pmp_fetch pmpcfg m (addr_of_intptr (get_reg_in_set rs PC)) instr_size) with
    | Some _ =>
        match i with
        | Iadd rd r1 r2 =>
            let v := Intptr.add (get_reg_in_set rs r1) (get_reg_in_set rs r2) in
            let rs_rd := (get_reg_in_set rs rd) in
            Next (nextinstr (set_reg_in_set rs rd (Intptr_to_int v))) m None
        | Isub rd r1 r2 =>
            let v := Intptr.sub (get_reg_in_set rs r1) (get_reg_in_set rs r2) in
            Next (nextinstr (set_reg_in_set rs rd (Intptr_to_int v))) m None
        | Ili rd v => Next (nextinstr (set_reg_in_set rs rd (Intptr_to_int v))) m None
        | Ilb rd r1 ofs =>
            let addr := addr_of_intptr (Intptr.add (get_reg_in_set rs r1) ofs) in
            let pmpa := MLoad addr 1 in
            match (pmp_load pmpcfg m addr 1) with
            | Some lv =>
                let v := Intptr.sign_ext 8 (Intptr.repr (decode_int lv)) in
                (* CHANGER PAR BARRAY MODIF ici c'est écrit rs[rd] prend val v *)
                Next (nextinstr (set_reg_in_set rs rd (Intptr_to_int v))) m (Some pmpa)
            | None => Exception (ExcLoadAccessFault pmpa)
            end
        | Isb r1 r2 ofs =>
            let addr := addr_of_intptr (Intptr.add (get_reg_in_set rs r2) ofs) in
            let lv := encode_int 1 (Intptr.unsigned (get_reg_in_set rs r1)) in
            let pmpa := MStore addr lv in
            match (pmp_store pmpcfg m addr lv) with
            | Some m' => Next (nextinstr rs) m' (Some pmpa)
            | None => Exception (ExcStoreAccessFault pmpa)
            end
        | Ilabel lbl => Next (nextinstr rs) m None
        | Ijump lbl =>
            match (PTree.get lbl ge) with
            | Some addr => Next (set_reg_in_set rs PC (Intptr_to_int addr)) m None
            | None => Stuck
            end
        | Iecall c => Exception (ExcEcall c)
        | Iuret => Exception (ExcIllegalInstr OpUret)
        end
    | None => Stuck
    end.

End Semantics.