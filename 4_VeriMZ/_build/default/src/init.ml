open Verimz.Machine
open Verimz.Memory
open Verimz.Pmp
open Verimz.Kernel
open Verimz.Oracle
open Verimz.Machine
open Verimz.Utils
open Verimz.Maps
open Verimz.Asm
open Verimz.Int0
open Monad
open List
open Print_state

open Verimz.Barocqmz

let mem_size : int = 1024

(* Machine *)
let machine_init : machine = { mtime = 0; mtimecmp = coq_Br_interface_QUANTUM_CYC; mie = 0 }

(* coq_Br_interface_QUANTUM_CYC pas trouvé !! *)

(* Zones *)

(* ======================================= *)
(* ZONE 1 (nouveau format : arrays = lists) *)
(* ======================================= *)

let zone1_pc : int = 0
let zone1_data : mem_addr = 256

let zone1_pmpcfg1 : pmp_register =
  { mode = NAPOT 8; perm_read = false; perm_write = false; perm_exec = true }

let zone1_pmpaddr1 : int = zone1_pc

let zone1_pmpcfg2 : pmp_register =
  { mode = NAPOT 8; perm_read = true; perm_write = true; perm_exec = false }

let zone1_pmpaddr2 : mem_addr = zone1_data

let zone1_pmp : pmp_config =
  [(zone1_pmpcfg1, zone1_pmpaddr1); (zone1_pmpcfg2, zone1_pmpaddr2)]

(* --- conversion regset -> array int (list) --- *)
(* On choisit la taille minimale 8 car PC est codé 7 dans votre Coq. *)
let regs_len = 8

(* si vous avez une fonction code_preg en OCaml, utilisez-la ; ici on met PC à l'indice 7 *)
let zone1_regs_init =
  let arr = Array.make regs_len 0 in
  arr.(7) <- zone1_pc;        (* affectation valide ici *)
  Array.to_list arr

(* --- pmpcfg/pmpaddr en arrays (lists d'int) --- *)
(* Il faut encoder pmp_register -> int si vous voulez conserver la config pleinement.
   Ici on met des placeholders 0 pour pmpcfg, et on met les adresses réelles pour pmpaddr. *)

(* placeholder : encodage à définir si nécessaire *)
let encode_pmp_register (_ : pmp_register) : int = 0

let zone1_pmpcfg_array : int list =
  List.map encode_pmp_register (List.map fst zone1_pmp)

let zone1_pmpaddr_array : int list =
  List.map (fun a -> a) (List.map snd zone1_pmp)
  (* ou : [zone1_pmpaddr1; zone1_pmpaddr2] *)

(* --- handlers : avant vous utilisiez [make 8 None], maintenant type = array int *)
let zone1_excpt_handlers : int list =  List.init 8 (fun _ -> 0)


let truc : truc_bidule_kernel = {truc_bidule_kernel_dummy = 0;}

(* --- construction finale de la zone conforme à Br_interface_zone --- *)
(* attention, par Br_interface_zone mais br_... b et pas B *)
let zone1 : br_interface_zone =
  {
    br_interface_zone_id = 0;
    br_interface_zone_regs = zone1_regs_init;
    br_interface_zone_pmpcfg = zone1_pmpcfg_array;
    br_interface_zone_pmpaddr = zone1_pmpaddr_array;
    br_interface_zone_ustatus = 0;
    br_interface_zone_uepc = 0;
    br_interface_zone_uie = 0;
    br_interface_zone_ucause = 0;
    br_interface_zone_utval = 0;
    br_interface_zone_excpt_handlers = zone1_excpt_handlers;
    br_interface_zone_intrp_deleg = 0;
  }

(* MODIF FIN *)




let z1_main_label : label = 1

let z1_loop_label : label = 2

let z1_excpt_0x7_handler_label : label = 3

let z1_main_label_offset : int = instr_size * 6

let z1_main_label_addr : mem_addr = zone1_pc + z1_main_label_offset

let z1_loop_label_offset : int = instr_size * 14

let z1_loop_label_addr : mem_addr = zone1_pc + z1_loop_label_offset

let z1_excpt_0x7_handler_label_offset : int = instr_size * 1

let z1_excpt_0x7_handler_label_addr : mem_addr =
  zone1_pc + z1_excpt_0x7_handler_label_offset

let zone1_genv : genv =
  PTree.set z1_main_label z1_main_label_addr PTree.Empty
  |> PTree.set z1_loop_label z1_loop_label_addr
  |> PTree.set z1_excpt_0x7_handler_label z1_excpt_0x7_handler_label_addr

let zone1_code : code =
  [
    Ijump z1_main_label;
    (* == Store access fault exception handler *)
    Ilabel z1_excpt_0x7_handler_label;
    Ili (X1, 300);
    Ili (X2, 260);
    Isb (X1, X2, 0);
    Iuret;
    (* ============ Main function ============ *)
    Ilabel z1_main_label;
    Ili (X1, 5);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Ili (X2, 400);
    Isb (X1, X2, 0);
    Iecall (EcallExcptVect (7, z1_excpt_0x7_handler_label_addr));
    Ilabel z1_loop_label;
    Ili (X1, 6);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Ili (X3, 999);
    Isb (X1, X3, 1);
    Ijump z1_loop_label;
  ]

let zone1_prog : program = { prog_code = zone1_code; prog_genv = zone1_genv }

let zone1_descr : zone_descr = { zd_bpc = zone1_pc; zd_prog = zone1_prog }

(* ======================================= *)
(* ZONE 2 *)
(* ======================================= *)

let zone2_pc : int = 512

let zone2_data : mem_addr = 768

let zone2_pmpcfg1 : pmp_register =
  { mode = NAPOT 8; perm_read = false; perm_write = false; perm_exec = true }

let zone2_pmpaddr1 : mem_addr = zone2_pc

let zone2_pmpcfg2 : pmp_register =
  { mode = NAPOT 8; perm_read = true; perm_write = true; perm_exec = false }

let zone2_pmpaddr2 : mem_addr = zone2_data

let pmp_zone2 : pmp_config =
  [(zone2_pmpcfg1, zone2_pmpaddr1); (zone2_pmpcfg2, zone2_pmpaddr2)]

let zone2_regs_init : regset =
  let regs = Pregmap.init 0 in
  Pregmap.set PC zone2_pc regs

let zone2 : zone =
  {
    zn_id = 1;
    zn_regs = zone2_regs_init;
    zn_pmpcfg = pmp_zone2;
    zn_uepc = 0;
    zn_ustatus = 0;
    zn_uie = 0;
    zn_excpt_handlers = make 8 None;
  }

let z2_main_label : label = 1

let z2_loop_label : label = 2

let z2_intrp_0x3_handler_label : label = 3

let z2_main_label_offset : int = instr_size * 6

let z2_main_label_addr : mem_addr = zone2_pc + z2_main_label_offset

let z2_loop_label_offset : int = instr_size * 15

let z2_loop_label_addr : mem_addr = zone2_pc + z2_loop_label_offset

let z2_intrp_0x3_handler_label_offset : int = instr_size * 1

let z2_intrp_0x3_handler_label_addr : mem_addr =
  zone2_pc + z2_intrp_0x3_handler_label_offset

let zone2_genv : genv =
  PTree.set z2_main_label z2_main_label_addr PTree.Empty
  |> PTree.set z2_loop_label z2_loop_label_addr
  |> PTree.set z2_intrp_0x3_handler_label z2_intrp_0x3_handler_label_addr

let zone2_code : code =
  [
    Ijump z2_main_label;
    (* == Machine software interrupt handler = *)
    Ilabel z2_intrp_0x3_handler_label;
    Ili (X1, 300);
    Ili (X2, 896);
    Isb (X1, X2, 0);
    Iuret;
    (* ============ Main function ============ *)
    Ilabel z2_main_label;
    Iecall EcallCsrsMie;
    Ili (X1, 7);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Ili (X2, 768);
    Isb (X1, X2, 0);
    Iecall (EcallIntrpVect (3, z2_intrp_0x3_handler_label_addr));
    Ilabel z2_loop_label;
    Ili (X1, 8);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Iadd (X1, X1, X1);
    Ili (X3, 900);
    Isb (X1, X3, 1);
    Ijump z2_loop_label;
  ]

let zone2_prog : program = { prog_code = zone2_code; prog_genv = zone2_genv }

let zone2_descr : zone_descr = { zd_bpc = zone2_pc; zd_prog = zone2_prog }

(* Kernel *)

let kernel_init : kernel =
  {
    k_current = 0;
    k_deadline = 0;
    k_next = 0;
    k_next_quantum = 0;
    k_mie = 0;
    k_zones = [zone1; zone2];
    k_intrp_deleg = false;
    k_intrp_handlers =
      make 3 None @ [Some { ih_zid = 1; ih_handler = None }] @ make 7 None;
  }

let kernel_state_init : kernel_state =
  { ks_mach = machine_init; ks_ker = kernel_init }

(* State Machine *)

let mem_init : mem = make mem_size 0

let state_init : state =
  KernelState
    (InitEntry, kernel_state_init, [zone1_descr; zone2_descr], mem_init)

let random_timer () = Random.int 16 + 5

let random_zone_event () : zone_event =
  let rd = Random.int 6 in
  if rd >= 4 then
    let iev = if rd = 5 then IevMachineExternal else IevMachineSoftware in
    ZevInterrupt (iev, random_timer ())
  else ZevTimerUpdate (random_timer ())

let fuel = 200

let gen_kernel_events () = init fuel (fun _ -> random_timer ())

let gen_zone_events () = init fuel (fun _ -> random_zone_event ())

let contains_interrupts (l : zone_event list) : bool =
  List.fold_left
    (fun acc a ->
      match a with
      | ZevInterrupt _ -> acc || true
      | ZevTimerUpdate _ -> acc)
    false
    l

let oracle_init () =
  let z1_events = gen_zone_events () in
  let z2_events = gen_zone_events () in
  let orc =
    { kevents = gen_kernel_events (); zevents = [z1_events; z2_events] }
  in
  (* Printf.printf "Zone 1 events: %s\n" (zevents_to_string z1_events);
     Printf.printf "Zone 2 events: %s\n" (zevents_to_string z2_events); *)
  orc
