

From Verimz Require Import Int Machine Memory Pmp Barocqmzutils. 
From Coq Require Import Bool List BinIntDef.
From compcert Require Import Integers.
From BarocqComp Require Import Error Barray Intop.
Import BoolNotations ListNotations.

Open Scope error_monad_scope.
(** * Event oracle. It generates timer events used to update the [mtime] register of the underlying machine,
      as well as interrupt events for zones. Timer interrupts are triggered by comparing
      the [mtimecmp] and [mtime] registers, so they are not part of interrupt events. *)

Definition timer_event := int64.

Inductive interrupt_event :=
  | IevMachineExternal : interrupt_event
  | IevMachineSoftware : interrupt_event.

Inductive zone_event :=
  | ZevTimerUpdate : timer_event -> zone_event
  | ZevInterrupt : interrupt_event -> timer_event -> zone_event.

Record oracle := mk_oracle {
  kevents: list timer_event;
  zevents: list (list zone_event)
}.

(* Definition call_koracle (orc: oracle) : option (timer_event * oracle) :=
  let kevts := kevents orc in
  do hd <- hd_error kevts;
  Some (hd, mk_oracle (tl kevts) (zevents orc)). *)
(* 
  Definition call_koracle (orc: oracle) : res (timer_event * oracle) :=
  match hd_error (kevents orc) with
  | Some hd => OK (hd, mk_oracle (tl (kevents orc)) (zevents orc))
  | None => Error nil
  end. *)

(* 
cannot be defined here cuz needs res but iply doudlbe do definition and conflict
between do barocq/compcertCE definition and the one in veriMZ   
Ah nan enfait c'est bon en redéfinissant tout sans utiliser l'autre do de veriMZ qui
empêche le DO de compecertCE et de res (qui remplace option)
  *)

(* Definition call_zoracle (orc: oracle) (n: intptr) : option (zone_event * oracle) :=
  do zevts <- get2 (zevents orc) n;
  do zevents' <- set2 (zevents orc) n (tl zevts);
  do hd <- hd_error zevts;
  Some (hd, mk_oracle (kevents orc) zevents'). *)

Definition Intptr_to_int (p : Intptr.int) : Int.int :=
  Int.repr (Intptr.unsigned p).


    Definition hd_res {A : Type} (l : list A) : res A :=
  match l with
  | []    => Error nil
  | x::_  => OK x
  end.





    Definition call_koracle (orc: oracle) : res (timer_event * oracle) :=
      let kevts := kevents orc in
      do hd <- hd_res kevts;
      OK (hd, mk_oracle (tl kevts) (zevents orc)).

    Definition call_zoracle (orc: oracle) (n_int: int) : res (zone_event * oracle) :=
    let n := int_to_int64 n_int in
      do zevts <- get (zevents orc) n;
      do zevents' <- set (zevents orc) n (tl zevts);
      do hd <- hd_res zevts;
      OK (hd, mk_oracle (kevents orc) zevents').







  (* à faire avec zorcale aussi *)

(* Definition call_zoracle (orc : oracle) (n : Intptr.int): res (zone_event * oracle) :=
  let idx := Intptr_to_int n in

  (* 1. lecture de la liste des zevents à la position [idx] *)
  match get (zevents orc) idx with
  | Some zevents => OK (zevents, mk_oracle (tl (kevents orc)) (zevents orc))
  | None => Error nil
  end. *)


(*   
  | None =>
      Error nil
  | Some zevts =>
    (* 2. mise à jour : remplacer la liste par [tl zevts] *)
    match set (zevents orc) idx (tl zevts) with
    | None =>
        Error nil
    | Some zevents' =>
      (* 3. extraction de la tête de la liste originale *)
      match zevts with
      | [] =>
          Error nil
      | hd :: _ =>
          (* 4. succès : on retourne l’événement et le nouvel oracle *)
          OK (hd, mk_oracle (kevents orc) zevents')
      end
    end
  end. *)

  (*
Definition call_zoracle (orc: oracle) (n: intptr) : res (zone_event * oracle) :=
  (* 1. Récupérer la liste des événements de zone *)
  match get (zevents orc) (Intptr_to_int n) with
  | None =>
      (* l’indice hors bornes *)
      Error nil
  | Some zevts =>
      (* 2. Mettre à jour la liste en retirant le premier élément *)
      match set (zevents orc) (Intptr.unsigned_to_nat n) (tl zevts) with
      | None =>
          (* échec de la mise à jour *)
          Error nil
      | Some zevents' =>
          (* 3. Extraire la tête de la liste précédente *)
          match hd_error zevts with
          | None =>
              (* liste vide : pas d’événement *)
              Error nil
          | Some hd =>
              (* 4. Succès : on construit le nouvel oracle et on renvoie *)
              OK (hd, mk_oracle (kevents orc) zevents')
          end
      end
  end. *)