open Barocqmzutils
open Barray
open Errors
open Integers
open Intop
open Machine
open Monads
open RecordSet

type coq_Br_interface_inbox = { br_interface_inbox_full : Int.int;
                                br_interface_inbox_msgs : Int.int array }

type coq_Br_interface_zone = { br_interface_zone_id : Int.int;
                               br_interface_zone_regs : Int.int array;
                               br_interface_zone_pmpcfg : Int.int array;
                               br_interface_zone_pmpaddr : Int.int array;
                               br_interface_zone_ustatus : Int.int;
                               br_interface_zone_uepc : Int.int;
                               br_interface_zone_uie : Int.int;
                               br_interface_zone_ucause : Int.int;
                               br_interface_zone_utval : Int.int;
                               br_interface_zone_excpt_handlers : Int.int
                                                                  array;
                               br_interface_zone_intrp_deleg : Int.int }

type coq_Br_interface_intrp_handler = { br_interface_intrp_handler_zone : 
                                        Int.int;
                                        br_interface_intrp_handler_handler : 
                                        Int.int }

type coq_Br_interface_state = machine

type coq_Br_interface_kernel = { br_interface_kernel_current : Int.int;
                                 br_interface_kernel_deadline : Int64.int;
                                 br_interface_kernel_next : Int.int;
                                 br_interface_kernel_next_quantum : Int64.int;
                                 br_interface_kernel_mie : Int.int;
                                 br_interface_kernel_zones : coq_Br_interface_zone
                                                             array;
                                 br_interface_kernel_intrp_handlers : 
                                 coq_Br_interface_intrp_handler array;
                                 br_interface_kernel_inboxes : coq_Br_interface_inbox
                                                               array;
                                 br_interface_kernel_ms : coq_Br_interface_state }

(** val coq_Br_interface_QUANTUM_CYC : Int64.int **)

let coq_Br_interface_QUANTUM_CYC = 100

(** val coq_Br_interface_N_ZONES : Int.int **)

let coq_Br_interface_N_ZONES =
  failwith "AXIOM TO BE REALIZED (Verimz.Barocqmz.Br_interface_N_ZONES)"

(** val coq_Br_interface_read_mtime : machine -> Int64.int **)

let coq_Br_interface_read_mtime =
  read_mtime

(** val coq_Br_interface_write_mtimecmp : machine -> Int64.int -> machine **)

let coq_Br_interface_write_mtimecmp =
  write_mtimecmp

(** val coq_Br_interface_csr_write :
    coq_Br_interface_state -> Int.int -> Int.int -> coq_Br_interface_state res **)

let coq_Br_interface_csr_write ms _ val0 =
  MonError.ret (write_mie ms (int_to_Intptr val0))

(** val coq_Br_interface_MCAUSE_INTRP : Int.int -> Int.int res **)

let coq_Br_interface_MCAUSE_INTRP mcause =
  OK (Int.shru (Int.shl mcause (Int.repr 1)) (Int.repr 1))

(** val coq_Br_interface_binary_or : Int.int -> Int.int -> Int.int res **)

let coq_Br_interface_binary_or a b =
  OK (Int.coq_or a b)

(** val coq_Br_interface_USTATUS_IE : Int.int **)

let coq_Br_interface_USTATUS_IE =
  Int.repr 1

(** val coq_Br_interface_USTATUS_PIE : Int.int **)

let coq_Br_interface_USTATUS_PIE =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))

(** val coq_Br_interface_MIE_TIMER : Int.int **)

let coq_Br_interface_MIE_TIMER =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))

(** val coq_Br_interface_mie : Int.int **)

let coq_Br_interface_mie =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p)
    1)))))))))

(** val coq_Br_interface_PC : Int.int **)

let coq_Br_interface_PC =
  Int.repr 0

(** val coq_Br_interface_and_with_bitwise : Int.int -> Int.int -> Int.int **)

let coq_Br_interface_and_with_bitwise p_mie p_uie =
  let u_bitwise = Int.not p_uie in Int.coq_and p_mie u_bitwise

(** val coq_Br_sched_sched_yield :
    coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_sched_sched_yield p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  let u_current_id = p_ks.br_interface_kernel_current in
  MonError.bind (get u_zones (U64.of_u32 u_current_id)) (fun u_current ->
    MonError.bind (Barray.set u_zones (U64.of_u32 u_current_id) u_current)
      (fun u_zones0 ->
      let u_ks =
        set (fun b -> b.br_interface_kernel_zones) (fun f ->
          let a = fun r -> f r.br_interface_kernel_zones in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
          br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks
      in
      let u_ks0 =
        set (fun b -> b.br_interface_kernel_current) (fun f ->
          let i = fun r -> f r.br_interface_kernel_current in
          (fun x -> { br_interface_kernel_current = (i x);
          br_interface_kernel_deadline = x.br_interface_kernel_deadline;
          br_interface_kernel_next = x.br_interface_kernel_next;
          br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones =
          x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ ->
          u_ks.br_interface_kernel_next) u_ks
      in
      let u_ks1 =
        set (fun b -> b.br_interface_kernel_deadline) (fun f ->
          let i = fun r -> f r.br_interface_kernel_deadline in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          (i x); br_interface_kernel_next = x.br_interface_kernel_next;
          br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones =
          x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ ->
          Int64.add u_ks0.br_interface_kernel_deadline
            u_ks0.br_interface_kernel_next_quantum) u_ks0
      in
      let u_current_index = u_ks1.br_interface_kernel_current in
      MonError.bind
        (U32.coq_mod (Int.add u_current_index (Int.repr 1))
          coq_Br_interface_N_ZONES) (fun u_next_index ->
        let u_ks2 =
          set (fun b -> b.br_interface_kernel_next) (fun f ->
            let i = fun r -> f r.br_interface_kernel_next in
            (fun x -> { br_interface_kernel_current =
            x.br_interface_kernel_current; br_interface_kernel_deadline =
            x.br_interface_kernel_deadline; br_interface_kernel_next = 
            (i x); br_interface_kernel_next_quantum =
            x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
            x.br_interface_kernel_mie; br_interface_kernel_zones =
            x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
            x.br_interface_kernel_intrp_handlers;
            br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
            br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
            u_next_index) u_ks1
        in
        let u_ks3 =
          set (fun b -> b.br_interface_kernel_next_quantum) (fun f ->
            let i = fun r -> f r.br_interface_kernel_next_quantum in
            (fun x -> { br_interface_kernel_current =
            x.br_interface_kernel_current; br_interface_kernel_deadline =
            x.br_interface_kernel_deadline; br_interface_kernel_next =
            x.br_interface_kernel_next; br_interface_kernel_next_quantum =
            (i x); br_interface_kernel_mie = x.br_interface_kernel_mie;
            br_interface_kernel_zones = x.br_interface_kernel_zones;
            br_interface_kernel_intrp_handlers =
            x.br_interface_kernel_intrp_handlers;
            br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
            br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
            coq_Br_interface_QUANTUM_CYC) u_ks2
        in
        let u_ms = u_ks3.br_interface_kernel_ms in
        let u_ms0 =
          coq_Br_interface_write_mtimecmp u_ms
            u_ks3.br_interface_kernel_deadline
        in
        MonError.ret
          (set (fun b -> b.br_interface_kernel_ms) (fun f ->
            let b = fun r -> f r.br_interface_kernel_ms in
            (fun x -> { br_interface_kernel_current =
            x.br_interface_kernel_current; br_interface_kernel_deadline =
            x.br_interface_kernel_deadline; br_interface_kernel_next =
            x.br_interface_kernel_next; br_interface_kernel_next_quantum =
            x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
            x.br_interface_kernel_mie; br_interface_kernel_zones =
            x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
            x.br_interface_kernel_intrp_handlers;
            br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
            br_interface_kernel_ms = (b x) })) (fun _ -> u_ms0) u_ks3))))

(** val coq_Br_sched_sched_init :
    coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_sched_sched_init p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  let u_current_index = p_ks.br_interface_kernel_current in
  MonError.bind (get u_zones (U64.of_u32 u_current_index)) (fun u_current ->
    let u_current0 =
      set (fun b -> b.br_interface_zone_intrp_deleg) (fun f ->
        let i = fun r -> f r.br_interface_zone_intrp_deleg in
        (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
        br_interface_zone_regs = x.br_interface_zone_regs;
        br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
        br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
        br_interface_zone_ustatus = x.br_interface_zone_ustatus;
        br_interface_zone_uepc = x.br_interface_zone_uepc;
        br_interface_zone_uie = x.br_interface_zone_uie;
        br_interface_zone_ucause = x.br_interface_zone_ucause;
        br_interface_zone_utval = x.br_interface_zone_utval;
        br_interface_zone_excpt_handlers =
        x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
        (i x) })) (fun _ -> Int.repr 0) u_current
    in
    MonError.bind
      (Barray.set u_zones (U64.of_u32 u_current_index) u_current0)
      (fun u_zones0 ->
      let u_ks =
        set (fun b -> b.br_interface_kernel_zones) (fun f ->
          let a = fun r -> f r.br_interface_kernel_zones in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
          br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks
      in
      let u_ks0 =
        set (fun b -> b.br_interface_kernel_current) (fun f ->
          let i = fun r -> f r.br_interface_kernel_current in
          (fun x -> { br_interface_kernel_current = (i x);
          br_interface_kernel_deadline = x.br_interface_kernel_deadline;
          br_interface_kernel_next = x.br_interface_kernel_next;
          br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones =
          x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ -> Int.repr 0) u_ks
      in
      MonError.bind
        (let b0 = coq_Br_interface_read_mtime u_ks0.br_interface_kernel_ms in
         MonError.ret
           (set (fun b -> b.br_interface_kernel_deadline) (fun f ->
             let i = fun r -> f r.br_interface_kernel_deadline in
             (fun x -> { br_interface_kernel_current =
             x.br_interface_kernel_current; br_interface_kernel_deadline =
             (i x); br_interface_kernel_next = x.br_interface_kernel_next;
             br_interface_kernel_next_quantum =
             x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
             x.br_interface_kernel_mie; br_interface_kernel_zones =
             x.br_interface_kernel_zones;
             br_interface_kernel_intrp_handlers =
             x.br_interface_kernel_intrp_handlers;
             br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
             br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
             Int64.add b0 coq_Br_interface_QUANTUM_CYC) u_ks0)) (fun u_ks1 ->
        let u_current_index0 = u_ks1.br_interface_kernel_current in
        MonError.bind
          (U32.coq_mod (Int.add u_current_index0 (Int.repr 1))
            coq_Br_interface_N_ZONES) (fun u_next_index ->
          let u_ks2 =
            set (fun b -> b.br_interface_kernel_next) (fun f ->
              let i = fun r -> f r.br_interface_kernel_next in
              (fun x -> { br_interface_kernel_current =
              x.br_interface_kernel_current; br_interface_kernel_deadline =
              x.br_interface_kernel_deadline; br_interface_kernel_next =
              (i x); br_interface_kernel_next_quantum =
              x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
              x.br_interface_kernel_mie; br_interface_kernel_zones =
              x.br_interface_kernel_zones;
              br_interface_kernel_intrp_handlers =
              x.br_interface_kernel_intrp_handlers;
              br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
              br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
              u_next_index) u_ks1
          in
          let u_ks3 =
            set (fun b -> b.br_interface_kernel_next_quantum) (fun f ->
              let i = fun r -> f r.br_interface_kernel_next_quantum in
              (fun x -> { br_interface_kernel_current =
              x.br_interface_kernel_current; br_interface_kernel_deadline =
              x.br_interface_kernel_deadline; br_interface_kernel_next =
              x.br_interface_kernel_next; br_interface_kernel_next_quantum =
              (i x); br_interface_kernel_mie = x.br_interface_kernel_mie;
              br_interface_kernel_zones = x.br_interface_kernel_zones;
              br_interface_kernel_intrp_handlers =
              x.br_interface_kernel_intrp_handlers;
              br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
              br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
              coq_Br_interface_QUANTUM_CYC) u_ks2
          in
          let u_ks4 =
            set (fun b -> b.br_interface_kernel_mie) (fun f ->
              let i = fun r -> f r.br_interface_kernel_mie in
              (fun x -> { br_interface_kernel_current =
              x.br_interface_kernel_current; br_interface_kernel_deadline =
              x.br_interface_kernel_deadline; br_interface_kernel_next =
              x.br_interface_kernel_next; br_interface_kernel_next_quantum =
              x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
              (i x); br_interface_kernel_zones = x.br_interface_kernel_zones;
              br_interface_kernel_intrp_handlers =
              x.br_interface_kernel_intrp_handlers;
              br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
              br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
              coq_Br_interface_MIE_TIMER) u_ks3
          in
          let u_ms = u_ks4.br_interface_kernel_ms in
          let u_ms0 =
            coq_Br_interface_write_mtimecmp u_ms
              u_ks4.br_interface_kernel_deadline
          in
          let u_ks5 =
            set (fun b -> b.br_interface_kernel_ms) (fun f ->
              let b = fun r -> f r.br_interface_kernel_ms in
              (fun x -> { br_interface_kernel_current =
              x.br_interface_kernel_current; br_interface_kernel_deadline =
              x.br_interface_kernel_deadline; br_interface_kernel_next =
              x.br_interface_kernel_next; br_interface_kernel_next_quantum =
              x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
              x.br_interface_kernel_mie; br_interface_kernel_zones =
              x.br_interface_kernel_zones;
              br_interface_kernel_intrp_handlers =
              x.br_interface_kernel_intrp_handlers;
              br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
              br_interface_kernel_ms = (b x) })) (fun _ -> u_ms0) u_ks4
          in
          MonError.bind
            (coq_Br_interface_csr_write u_ks5.br_interface_kernel_ms
              coq_Br_interface_mie u_ks5.br_interface_kernel_mie)
            (fun u_ms1 ->
            MonError.ret
              (set (fun b -> b.br_interface_kernel_ms) (fun f ->
                let b = fun r -> f r.br_interface_kernel_ms in
                (fun x -> { br_interface_kernel_current =
                x.br_interface_kernel_current; br_interface_kernel_deadline =
                x.br_interface_kernel_deadline; br_interface_kernel_next =
                x.br_interface_kernel_next;
                br_interface_kernel_next_quantum =
                x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
                x.br_interface_kernel_mie; br_interface_kernel_zones =
                x.br_interface_kernel_zones;
                br_interface_kernel_intrp_handlers =
                x.br_interface_kernel_intrp_handlers;
                br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
                br_interface_kernel_ms = (b x) })) (fun _ -> u_ms1) u_ks5))))))

(** val coq_Br_sched_sched_intrp :
    coq_Br_interface_kernel -> Int.int -> coq_Br_interface_kernel res **)

let coq_Br_sched_sched_intrp p_ks p_zone_id =
  let u_ks =
    set (fun b -> b.br_interface_kernel_next) (fun f ->
      let i = fun r -> f r.br_interface_kernel_next in
      (fun x -> { br_interface_kernel_current =
      x.br_interface_kernel_current; br_interface_kernel_deadline =
      x.br_interface_kernel_deadline; br_interface_kernel_next = (i x);
      br_interface_kernel_next_quantum = x.br_interface_kernel_next_quantum;
      br_interface_kernel_mie = x.br_interface_kernel_mie;
      br_interface_kernel_zones = x.br_interface_kernel_zones;
      br_interface_kernel_intrp_handlers =
      x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
      x.br_interface_kernel_inboxes; br_interface_kernel_ms =
      x.br_interface_kernel_ms })) (fun _ ->
      p_ks.br_interface_kernel_current) p_ks
  in
  let u_ks0 =
    set (fun b -> b.br_interface_kernel_current) (fun f ->
      let i = fun r -> f r.br_interface_kernel_current in
      (fun x -> { br_interface_kernel_current = (i x);
      br_interface_kernel_deadline = x.br_interface_kernel_deadline;
      br_interface_kernel_next = x.br_interface_kernel_next;
      br_interface_kernel_next_quantum = x.br_interface_kernel_next_quantum;
      br_interface_kernel_mie = x.br_interface_kernel_mie;
      br_interface_kernel_zones = x.br_interface_kernel_zones;
      br_interface_kernel_intrp_handlers =
      x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
      x.br_interface_kernel_inboxes; br_interface_kernel_ms =
      x.br_interface_kernel_ms })) (fun _ -> p_zone_id) u_ks
  in
  let u_now = coq_Br_interface_read_mtime u_ks0.br_interface_kernel_ms in
  let u_ks1 =
    if Int64.ltu u_now u_ks0.br_interface_kernel_deadline
    then set (fun b -> b.br_interface_kernel_next_quantum) (fun f ->
           let i = fun r -> f r.br_interface_kernel_next_quantum in
           (fun x -> { br_interface_kernel_current =
           x.br_interface_kernel_current; br_interface_kernel_deadline =
           x.br_interface_kernel_deadline; br_interface_kernel_next =
           x.br_interface_kernel_next; br_interface_kernel_next_quantum =
           (i x); br_interface_kernel_mie = x.br_interface_kernel_mie;
           br_interface_kernel_zones = x.br_interface_kernel_zones;
           br_interface_kernel_intrp_handlers =
           x.br_interface_kernel_intrp_handlers;
           br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
           br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
           Int64.sub u_ks0.br_interface_kernel_deadline u_now) u_ks0
    else set (fun b -> b.br_interface_kernel_next_quantum) (fun f ->
           let i = fun r -> f r.br_interface_kernel_next_quantum in
           (fun x -> { br_interface_kernel_current =
           x.br_interface_kernel_current; br_interface_kernel_deadline =
           x.br_interface_kernel_deadline; br_interface_kernel_next =
           x.br_interface_kernel_next; br_interface_kernel_next_quantum =
           (i x); br_interface_kernel_mie = x.br_interface_kernel_mie;
           br_interface_kernel_zones = x.br_interface_kernel_zones;
           br_interface_kernel_intrp_handlers =
           x.br_interface_kernel_intrp_handlers;
           br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
           br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
           Int64.repr 0) u_ks0
  in
  MonError.bind
    (let b0 = coq_Br_interface_read_mtime u_ks1.br_interface_kernel_ms in
     MonError.ret
       (set (fun b -> b.br_interface_kernel_deadline) (fun f ->
         let i = fun r -> f r.br_interface_kernel_deadline in
         (fun x -> { br_interface_kernel_current =
         x.br_interface_kernel_current; br_interface_kernel_deadline = 
         (i x); br_interface_kernel_next = x.br_interface_kernel_next;
         br_interface_kernel_next_quantum =
         x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
         x.br_interface_kernel_mie; br_interface_kernel_zones =
         x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
         x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
         x.br_interface_kernel_inboxes; br_interface_kernel_ms =
         x.br_interface_kernel_ms })) (fun _ ->
         Int64.add b0 coq_Br_interface_QUANTUM_CYC) u_ks1)) (fun u_ks2 ->
    let u_ms = u_ks2.br_interface_kernel_ms in
    let u_ms0 =
      coq_Br_interface_write_mtimecmp u_ms u_ks2.br_interface_kernel_deadline
    in
    MonError.ret
      (set (fun b -> b.br_interface_kernel_ms) (fun f ->
        let b = fun r -> f r.br_interface_kernel_ms in
        (fun x -> { br_interface_kernel_current =
        x.br_interface_kernel_current; br_interface_kernel_deadline =
        x.br_interface_kernel_deadline; br_interface_kernel_next =
        x.br_interface_kernel_next; br_interface_kernel_next_quantum =
        x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
        x.br_interface_kernel_mie; br_interface_kernel_zones =
        x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
        x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
        x.br_interface_kernel_inboxes; br_interface_kernel_ms = (b x) }))
        (fun _ -> u_ms0) u_ks2))

(** val coq_Br_sched_sched :
    coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_sched_sched p_ks =
  MonError.bind
    (let b0 = coq_Br_interface_read_mtime p_ks.br_interface_kernel_ms in
     if Int64.cmpu Cle p_ks.br_interface_kernel_deadline b0
     then coq_Br_sched_sched_yield p_ks
     else MonError.ret p_ks) (fun u_ks ->
    MonError.bind
      (MonError.bind
        (get u_ks.br_interface_kernel_zones
          (U64.of_u32 u_ks.br_interface_kernel_current)) (fun b1 ->
        if Int.cmpu Cgt b1.br_interface_zone_intrp_deleg (Int.repr 0)
        then coq_Br_interface_csr_write u_ks.br_interface_kernel_ms
               coq_Br_interface_mie coq_Br_interface_MIE_TIMER
        else coq_Br_interface_csr_write u_ks.br_interface_kernel_ms
               coq_Br_interface_mie u_ks.br_interface_kernel_mie))
      (fun u_ms ->
      MonError.ret
        (set (fun b -> b.br_interface_kernel_ms) (fun f ->
          let b = fun r -> f r.br_interface_kernel_ms in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones =
          x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms = (b x) }))
          (fun _ -> u_ms) u_ks)))

(** val coq_Br_ecall_N_ECALLS : Int.int **)

let coq_Br_ecall_N_ECALLS =
  Int.repr ((fun p->2*p) ((fun p->1+2*p) 1))

(** val coq_Br_ecall_ECL_EXCPT_VECT : Int.int **)

let coq_Br_ecall_ECL_EXCPT_VECT =
  Int.repr 0

(** val coq_Br_ecall_ECL_INTRP_VECT : Int.int **)

let coq_Br_ecall_ECL_INTRP_VECT =
  Int.repr 1

(** val coq_Br_ecall_ECL_CSRS_MIE : Int.int **)

let coq_Br_ecall_ECL_CSRS_MIE =
  Int.repr ((fun p->2*p) 1)

(** val coq_Br_ecall_ECL_CSRC_MIE : Int.int **)

let coq_Br_ecall_ECL_CSRC_MIE =
  Int.repr ((fun p->1+2*p) 1)

(** val coq_Br_ecall_ECL_SEND : Int.int **)

let coq_Br_ecall_ECL_SEND =
  Int.repr ((fun p->2*p) ((fun p->2*p) 1))

(** val coq_Br_ecall_ECL_RECV : Int.int **)

let coq_Br_ecall_ECL_RECV =
  Int.repr ((fun p->1+2*p) ((fun p->2*p) 1))

(** val coq_Br_ecall_A0 : Int.int **)

let coq_Br_ecall_A0 =
  Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))

(** val coq_Br_ecall_A1 : Int.int **)

let coq_Br_ecall_A1 =
  Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) 1)))

(** val coq_Br_ecall_A2 : Int.int **)

let coq_Br_ecall_A2 =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))

(** val coq_Br_ecall_A3 : Int.int **)

let coq_Br_ecall_A3 =
  Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))

(** val coq_Br_ecall_A4 : Int.int **)

let coq_Br_ecall_A4 =
  Int.repr ((fun p->2*p) ((fun p->1+2*p) ((fun p->1+2*p) 1)))

(** val coq_Br_ecall_A7 : Int.int **)

let coq_Br_ecall_A7 =
  Int.repr ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1))))

(** val coq_Br_ecall_N_EXCEPTIONS : Int.int **)

let coq_Br_ecall_N_EXCEPTIONS =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))

(** val coq_Br_ecall_N_INTERRUPTS : Int.int **)

let coq_Br_ecall_N_INTERRUPTS =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->1+2*p) 1)))

(** val coq_Br_ecall_ecall_excpt_vect :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_excpt_vect p_caller_id p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 p_caller_id)) (fun u_caller ->
    let u_regs = u_caller.br_interface_zone_regs in
    MonError.bind (get u_regs (U64.of_u32 coq_Br_ecall_A0)) (fun u_code ->
      MonError.bind (get u_regs (U64.of_u32 coq_Br_ecall_A1))
        (fun u_handler ->
        MonError.bind
          (if Int.ltu u_code coq_Br_ecall_N_EXCEPTIONS
           then let u_excpt_handlers =
                  u_caller.br_interface_zone_excpt_handlers
                in
                MonError.bind
                  (Barray.set u_excpt_handlers (U64.of_u32 u_code) u_handler)
                  (fun u_excpt_handlers0 ->
                  MonError.ret
                    (set (fun b -> b.br_interface_zone_excpt_handlers)
                      (fun f ->
                      let a = fun r -> f r.br_interface_zone_excpt_handlers in
                      (fun x -> { br_interface_zone_id =
                      x.br_interface_zone_id; br_interface_zone_regs =
                      x.br_interface_zone_regs; br_interface_zone_pmpcfg =
                      x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
                      x.br_interface_zone_pmpaddr;
                      br_interface_zone_ustatus =
                      x.br_interface_zone_ustatus; br_interface_zone_uepc =
                      x.br_interface_zone_uepc; br_interface_zone_uie =
                      x.br_interface_zone_uie; br_interface_zone_ucause =
                      x.br_interface_zone_ucause; br_interface_zone_utval =
                      x.br_interface_zone_utval;
                      br_interface_zone_excpt_handlers = (a x);
                      br_interface_zone_intrp_deleg =
                      x.br_interface_zone_intrp_deleg })) (fun _ ->
                      u_excpt_handlers0) u_caller))
           else MonError.ret u_caller) (fun u_caller0 ->
          MonError.bind
            (Barray.set u_zones (U64.of_u32 p_caller_id) u_caller0)
            (fun u_zones0 ->
            MonError.ret
              (set (fun b -> b.br_interface_kernel_zones) (fun f ->
                let a = fun r -> f r.br_interface_kernel_zones in
                (fun x -> { br_interface_kernel_current =
                x.br_interface_kernel_current; br_interface_kernel_deadline =
                x.br_interface_kernel_deadline; br_interface_kernel_next =
                x.br_interface_kernel_next;
                br_interface_kernel_next_quantum =
                x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
                x.br_interface_kernel_mie; br_interface_kernel_zones = 
                (a x); br_interface_kernel_intrp_handlers =
                x.br_interface_kernel_intrp_handlers;
                br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
                br_interface_kernel_ms = x.br_interface_kernel_ms }))
                (fun _ -> u_zones0) p_ks))))))

(** val coq_Br_ecall_ecall_intrp_vect_step_2 :
    Int.int -> Int.int -> Int.int -> Int.int -> coq_Br_interface_kernel ->
    coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_intrp_vect_step_2 p_intrp_handler_zone_fielf_id p_caller_field_id p_code p_handler p_ks =
  let u_intrp_handlers = p_ks.br_interface_kernel_intrp_handlers in
  MonError.bind (get u_intrp_handlers (U64.of_u32 p_code))
    (fun u_intrp_handler ->
    let u_intrp_handler2 =
      if (&&) (Int.ltu p_code coq_Br_ecall_N_INTERRUPTS)
           (Int.eq p_intrp_handler_zone_fielf_id p_caller_field_id)
      then set (fun b -> b.br_interface_intrp_handler_handler) (fun f ->
             let i = fun r -> f r.br_interface_intrp_handler_handler in
             (fun x -> { br_interface_intrp_handler_zone =
             x.br_interface_intrp_handler_zone;
             br_interface_intrp_handler_handler = (i x) })) (fun _ ->
             p_handler) u_intrp_handler
      else u_intrp_handler
    in
    MonError.bind
      (Barray.set u_intrp_handlers (U64.of_u32 p_code) u_intrp_handler2)
      (fun u_intrp_handlers0 ->
      let u_ks2 =
        set (fun b -> b.br_interface_kernel_intrp_handlers) (fun f ->
          let a = fun r -> f r.br_interface_kernel_intrp_handlers in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones =
          x.br_interface_kernel_zones; br_interface_kernel_intrp_handlers =
          (a x); br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
          br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
          u_intrp_handlers0) p_ks
      in
      MonError.ret u_ks2))

(** val coq_Br_ecall_ecall_intrp_vect :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_intrp_vect p_caller_id p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 p_caller_id)) (fun u_caller ->
    let u_regs = u_caller.br_interface_zone_regs in
    MonError.bind (get u_regs (U64.of_u32 coq_Br_ecall_A0)) (fun u_code ->
      MonError.bind (get u_regs (U64.of_u32 coq_Br_ecall_A1))
        (fun u_handler ->
        let u_caller_field_id = u_caller.br_interface_zone_id in
        MonError.bind (Barray.set u_zones (U64.of_u32 p_caller_id) u_caller)
          (fun u_zones0 ->
          let u_ks1 =
            set (fun b -> b.br_interface_kernel_zones) (fun f ->
              let a = fun r -> f r.br_interface_kernel_zones in
              (fun x -> { br_interface_kernel_current =
              x.br_interface_kernel_current; br_interface_kernel_deadline =
              x.br_interface_kernel_deadline; br_interface_kernel_next =
              x.br_interface_kernel_next; br_interface_kernel_next_quantum =
              x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
              x.br_interface_kernel_mie; br_interface_kernel_zones = 
              (a x); br_interface_kernel_intrp_handlers =
              x.br_interface_kernel_intrp_handlers;
              br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
              br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
              u_zones0) p_ks
          in
          let u_intrp_handlers = u_ks1.br_interface_kernel_intrp_handlers in
          MonError.bind (get u_intrp_handlers (U64.of_u32 u_code))
            (fun u_intrp_handler ->
            let u_intrp_handler_zone_index =
              u_intrp_handler.br_interface_intrp_handler_zone
            in
            MonError.bind
              (Barray.set u_intrp_handlers (U64.of_u32 u_code)
                u_intrp_handler) (fun u_intrp_handlers0 ->
              let u_ks2 =
                set (fun b -> b.br_interface_kernel_intrp_handlers) (fun f ->
                  let a = fun r -> f r.br_interface_kernel_intrp_handlers in
                  (fun x -> { br_interface_kernel_current =
                  x.br_interface_kernel_current;
                  br_interface_kernel_deadline =
                  x.br_interface_kernel_deadline; br_interface_kernel_next =
                  x.br_interface_kernel_next;
                  br_interface_kernel_next_quantum =
                  x.br_interface_kernel_next_quantum;
                  br_interface_kernel_mie = x.br_interface_kernel_mie;
                  br_interface_kernel_zones = x.br_interface_kernel_zones;
                  br_interface_kernel_intrp_handlers = (a x);
                  br_interface_kernel_inboxes =
                  x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                  x.br_interface_kernel_ms })) (fun _ -> u_intrp_handlers0)
                  u_ks1
              in
              let u_zones1 = u_ks2.br_interface_kernel_zones in
              MonError.bind
                (get u_zones1 (U64.of_u32 u_intrp_handler_zone_index))
                (fun u_intrp_handler_zone ->
                let u_intrp_handler_zone_fielf_id =
                  u_intrp_handler_zone.br_interface_zone_id
                in
                MonError.bind
                  (Barray.set u_zones1
                    (U64.of_u32 u_intrp_handler_zone_index)
                    u_intrp_handler_zone) (fun u_zones2 ->
                  let u_ks3 =
                    set (fun b -> b.br_interface_kernel_zones) (fun f ->
                      let a = fun r -> f r.br_interface_kernel_zones in
                      (fun x -> { br_interface_kernel_current =
                      x.br_interface_kernel_current;
                      br_interface_kernel_deadline =
                      x.br_interface_kernel_deadline;
                      br_interface_kernel_next = x.br_interface_kernel_next;
                      br_interface_kernel_next_quantum =
                      x.br_interface_kernel_next_quantum;
                      br_interface_kernel_mie = x.br_interface_kernel_mie;
                      br_interface_kernel_zones = (a x);
                      br_interface_kernel_intrp_handlers =
                      x.br_interface_kernel_intrp_handlers;
                      br_interface_kernel_inboxes =
                      x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                      x.br_interface_kernel_ms })) (fun _ -> u_zones2) u_ks2
                  in
                  MonError.bind
                    (coq_Br_ecall_ecall_intrp_vect_step_2
                      u_intrp_handler_zone_fielf_id u_caller_field_id u_code
                      u_handler u_ks3) (fun u_ks4 ->
                    let u_zones3 = u_ks4.br_interface_kernel_zones in
                    MonError.bind (get u_zones3 (U64.of_u32 p_caller_id))
                      (fun u_caller0 ->
                      let u_caller1 =
                        if (&&) (Int.ltu u_code coq_Br_ecall_N_INTERRUPTS)
                             (Int.eq u_intrp_handler_zone_fielf_id
                               u_caller_field_id)
                        then set (fun b -> b.br_interface_zone_uie) (fun f ->
                               let i = fun r -> f r.br_interface_zone_uie in
                               (fun x -> { br_interface_zone_id =
                               x.br_interface_zone_id;
                               br_interface_zone_regs =
                               x.br_interface_zone_regs;
                               br_interface_zone_pmpcfg =
                               x.br_interface_zone_pmpcfg;
                               br_interface_zone_pmpaddr =
                               x.br_interface_zone_pmpaddr;
                               br_interface_zone_ustatus =
                               x.br_interface_zone_ustatus;
                               br_interface_zone_uepc =
                               x.br_interface_zone_uepc;
                               br_interface_zone_uie = (i x);
                               br_interface_zone_ucause =
                               x.br_interface_zone_ucause;
                               br_interface_zone_utval =
                               x.br_interface_zone_utval;
                               br_interface_zone_excpt_handlers =
                               x.br_interface_zone_excpt_handlers;
                               br_interface_zone_intrp_deleg =
                               x.br_interface_zone_intrp_deleg })) (fun _ ->
                               Int.coq_or u_caller0.br_interface_zone_uie
                                 (Int.shl (Int.repr 1) u_code)) u_caller0
                        else u_caller0
                      in
                      let u_ustatus = u_caller1.br_interface_zone_ustatus in
                      MonError.bind
                        (Barray.set u_zones3 (U64.of_u32 p_caller_id)
                          u_caller1) (fun u_zones4 ->
                        let u_ks5 =
                          set (fun b -> b.br_interface_kernel_zones)
                            (fun f ->
                            let a = fun r -> f r.br_interface_kernel_zones in
                            (fun x -> { br_interface_kernel_current =
                            x.br_interface_kernel_current;
                            br_interface_kernel_deadline =
                            x.br_interface_kernel_deadline;
                            br_interface_kernel_next =
                            x.br_interface_kernel_next;
                            br_interface_kernel_next_quantum =
                            x.br_interface_kernel_next_quantum;
                            br_interface_kernel_mie =
                            x.br_interface_kernel_mie;
                            br_interface_kernel_zones = (a x);
                            br_interface_kernel_intrp_handlers =
                            x.br_interface_kernel_intrp_handlers;
                            br_interface_kernel_inboxes =
                            x.br_interface_kernel_inboxes;
                            br_interface_kernel_ms =
                            x.br_interface_kernel_ms })) (fun _ -> u_zones4)
                            u_ks4
                        in
                        if (&&)
                             ((&&) (Int.ltu u_code coq_Br_ecall_N_INTERRUPTS)
                               (Int.eq u_intrp_handler_zone_fielf_id
                                 u_caller_field_id))
                             (Int.cmpu Cgt
                               (Int.coq_and u_ustatus
                                 coq_Br_interface_USTATUS_IE) (Int.repr 0))
                        then MonError.ret
                               (set (fun b -> b.br_interface_kernel_mie)
                                 (fun f ->
                                 let i = fun r -> f r.br_interface_kernel_mie
                                 in
                                 (fun x -> { br_interface_kernel_current =
                                 x.br_interface_kernel_current;
                                 br_interface_kernel_deadline =
                                 x.br_interface_kernel_deadline;
                                 br_interface_kernel_next =
                                 x.br_interface_kernel_next;
                                 br_interface_kernel_next_quantum =
                                 x.br_interface_kernel_next_quantum;
                                 br_interface_kernel_mie = (i x);
                                 br_interface_kernel_zones =
                                 x.br_interface_kernel_zones;
                                 br_interface_kernel_intrp_handlers =
                                 x.br_interface_kernel_intrp_handlers;
                                 br_interface_kernel_inboxes =
                                 x.br_interface_kernel_inboxes;
                                 br_interface_kernel_ms =
                                 x.br_interface_kernel_ms })) (fun _ ->
                                 Int.coq_or u_ks5.br_interface_kernel_mie
                                   (Int.shl (Int.repr 1) u_code)) u_ks5)
                        else MonError.ret u_ks5)))))))))))

(** val coq_Br_ecall_ecall_csrs_mie_CALLER :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_csrs_mie_CALLER p_caller_id p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 p_caller_id)) (fun u_caller ->
    let u_caller0 =
      set (fun b -> b.br_interface_zone_ustatus) (fun f ->
        let i = fun r -> f r.br_interface_zone_ustatus in
        (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
        br_interface_zone_regs = x.br_interface_zone_regs;
        br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
        br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
        br_interface_zone_ustatus = (i x); br_interface_zone_uepc =
        x.br_interface_zone_uepc; br_interface_zone_uie =
        x.br_interface_zone_uie; br_interface_zone_ucause =
        x.br_interface_zone_ucause; br_interface_zone_utval =
        x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
        x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
        x.br_interface_zone_intrp_deleg })) (fun _ ->
        Int.coq_or u_caller.br_interface_zone_ustatus
          coq_Br_interface_USTATUS_IE) u_caller
    in
    MonError.bind (Barray.set u_zones (U64.of_u32 p_caller_id) u_caller0)
      (fun u_zones0 ->
      MonError.ret
        (set (fun b -> b.br_interface_kernel_zones) (fun f ->
          let a = fun r -> f r.br_interface_kernel_zones in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
          br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks)))

(** val coq_Br_ecall_ecall_csrs_mie_KERNEL :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_csrs_mie_KERNEL p_caller_id p_ks =
  MonError.bind (get p_ks.br_interface_kernel_zones (U64.of_u32 p_caller_id))
    (fun u_caller ->
    MonError.ret
      (set (fun b -> b.br_interface_kernel_mie) (fun f ->
        let i = fun r -> f r.br_interface_kernel_mie in
        (fun x -> { br_interface_kernel_current =
        x.br_interface_kernel_current; br_interface_kernel_deadline =
        x.br_interface_kernel_deadline; br_interface_kernel_next =
        x.br_interface_kernel_next; br_interface_kernel_next_quantum =
        x.br_interface_kernel_next_quantum; br_interface_kernel_mie = 
        (i x); br_interface_kernel_zones = x.br_interface_kernel_zones;
        br_interface_kernel_intrp_handlers =
        x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
        x.br_interface_kernel_inboxes; br_interface_kernel_ms =
        x.br_interface_kernel_ms })) (fun _ ->
        Int.coq_or p_ks.br_interface_kernel_mie u_caller.br_interface_zone_uie)
        p_ks))

(** val coq_Br_ecall_ecall_csrc_mie_CALLER :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_csrc_mie_CALLER p_caller_id p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 p_caller_id)) (fun u_caller ->
    let u_caller0 =
      let b0 =
        coq_Br_interface_and_with_bitwise u_caller.br_interface_zone_ustatus
          coq_Br_interface_USTATUS_IE
      in
      set (fun b -> b.br_interface_zone_ustatus) (fun f ->
        let i = fun r -> f r.br_interface_zone_ustatus in
        (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
        br_interface_zone_regs = x.br_interface_zone_regs;
        br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
        br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
        br_interface_zone_ustatus = (i x); br_interface_zone_uepc =
        x.br_interface_zone_uepc; br_interface_zone_uie =
        x.br_interface_zone_uie; br_interface_zone_ucause =
        x.br_interface_zone_ucause; br_interface_zone_utval =
        x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
        x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
        x.br_interface_zone_intrp_deleg })) (fun _ -> b0) u_caller
    in
    MonError.bind (Barray.set u_zones (U64.of_u32 p_caller_id) u_caller0)
      (fun u_zones0 ->
      MonError.ret
        (set (fun b -> b.br_interface_kernel_zones) (fun f ->
          let a = fun r -> f r.br_interface_kernel_zones in
          (fun x -> { br_interface_kernel_current =
          x.br_interface_kernel_current; br_interface_kernel_deadline =
          x.br_interface_kernel_deadline; br_interface_kernel_next =
          x.br_interface_kernel_next; br_interface_kernel_next_quantum =
          x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
          x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
          br_interface_kernel_intrp_handlers =
          x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
          x.br_interface_kernel_inboxes; br_interface_kernel_ms =
          x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks)))

(** val coq_Br_ecall_ecall_csrc_mie_KERNEL :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_csrc_mie_KERNEL p_caller_id p_ks =
  MonError.bind (get p_ks.br_interface_kernel_zones (U64.of_u32 p_caller_id))
    (fun u_caller ->
    let b0 =
      coq_Br_interface_and_with_bitwise p_ks.br_interface_kernel_mie
        u_caller.br_interface_zone_uie
    in
    MonError.ret
      (set (fun b -> b.br_interface_kernel_mie) (fun f ->
        let i = fun r -> f r.br_interface_kernel_mie in
        (fun x -> { br_interface_kernel_current =
        x.br_interface_kernel_current; br_interface_kernel_deadline =
        x.br_interface_kernel_deadline; br_interface_kernel_next =
        x.br_interface_kernel_next; br_interface_kernel_next_quantum =
        x.br_interface_kernel_next_quantum; br_interface_kernel_mie = 
        (i x); br_interface_kernel_zones = x.br_interface_kernel_zones;
        br_interface_kernel_intrp_handlers =
        x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
        x.br_interface_kernel_inboxes; br_interface_kernel_ms =
        x.br_interface_kernel_ms })) (fun _ -> b0) p_ks))

(** val coq_Br_ecall_ecall_send :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_send p_caller_id p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 p_caller_id)) (fun u_caller ->
    let u_regs = u_caller.br_interface_zone_regs in
    MonError.bind (get u_regs (U64.of_u32 coq_Br_ecall_A0))
      (fun u_receiver ->
      MonError.bind
        (Barray.set u_regs (U64.of_u32 coq_Br_ecall_A0) (Int.repr 0))
        (fun u_regs0 ->
        MonError.bind (get u_regs0 (U64.of_u32 coq_Br_ecall_A1))
          (fun u_regsA1 ->
          MonError.bind (get u_regs0 (U64.of_u32 coq_Br_ecall_A2))
            (fun u_regsA2 ->
            MonError.bind (get u_regs0 (U64.of_u32 coq_Br_ecall_A3))
              (fun u_regsA3 ->
              MonError.bind (get u_regs0 (U64.of_u32 coq_Br_ecall_A4))
                (fun u_regsA4 ->
                let u_caller0 =
                  set (fun b -> b.br_interface_zone_regs) (fun f ->
                    let a = fun r -> f r.br_interface_zone_regs in
                    (fun x -> { br_interface_zone_id =
                    x.br_interface_zone_id; br_interface_zone_regs = 
                    (a x); br_interface_zone_pmpcfg =
                    x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
                    x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
                    x.br_interface_zone_ustatus; br_interface_zone_uepc =
                    x.br_interface_zone_uepc; br_interface_zone_uie =
                    x.br_interface_zone_uie; br_interface_zone_ucause =
                    x.br_interface_zone_ucause; br_interface_zone_utval =
                    x.br_interface_zone_utval;
                    br_interface_zone_excpt_handlers =
                    x.br_interface_zone_excpt_handlers;
                    br_interface_zone_intrp_deleg =
                    x.br_interface_zone_intrp_deleg })) (fun _ -> u_regs0)
                    u_caller
                in
                MonError.bind
                  (Barray.set u_zones (U64.of_u32 p_caller_id) u_caller0)
                  (fun u_zones0 ->
                  let u_ks =
                    set (fun b -> b.br_interface_kernel_zones) (fun f ->
                      let a = fun r -> f r.br_interface_kernel_zones in
                      (fun x -> { br_interface_kernel_current =
                      x.br_interface_kernel_current;
                      br_interface_kernel_deadline =
                      x.br_interface_kernel_deadline;
                      br_interface_kernel_next = x.br_interface_kernel_next;
                      br_interface_kernel_next_quantum =
                      x.br_interface_kernel_next_quantum;
                      br_interface_kernel_mie = x.br_interface_kernel_mie;
                      br_interface_kernel_zones = (a x);
                      br_interface_kernel_intrp_handlers =
                      x.br_interface_kernel_intrp_handlers;
                      br_interface_kernel_inboxes =
                      x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                      x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks
                  in
                  if Int.cmpu Cge u_receiver coq_Br_interface_N_ZONES
                  then MonError.ret u_ks
                  else let u_inboxes = u_ks.br_interface_kernel_inboxes in
                       let u_index =
                         Int.add
                           (Int.mul u_receiver coq_Br_interface_N_ZONES)
                           p_caller_id
                       in
                       MonError.bind (get u_inboxes (U64.of_u32 u_index))
                         (fun u_inbox ->
                         let u_full = u_inbox.br_interface_inbox_full in
                         let u_msgs = u_inbox.br_interface_inbox_msgs in
                         MonError.bind
                           (if Int.cmpu Cgt u_full (Int.repr 0)
                            then MonError.ret u_msgs
                            else MonError.bind
                                   (Barray.set u_msgs
                                     (U64.of_u32 (Int.repr 0)) u_regsA1)
                                   (fun u_msgs0 ->
                                   MonError.bind
                                     (Barray.set u_msgs0
                                       (U64.of_u32 (Int.repr 1)) u_regsA2)
                                     (fun u_msgs1 ->
                                     MonError.bind
                                       (Barray.set u_msgs1
                                         (U64.of_u32
                                           (Int.repr ((fun p->2*p) 1)))
                                         u_regsA3) (fun u_msgs2 ->
                                       Barray.set u_msgs2
                                         (U64.of_u32
                                           (Int.repr ((fun p->1+2*p) 1)))
                                         u_regsA4)))) (fun u_msgs0 ->
                           let u_inbox0 =
                             set (fun b -> b.br_interface_inbox_msgs)
                               (fun f ->
                               let a = fun r -> f r.br_interface_inbox_msgs in
                               (fun x -> { br_interface_inbox_full =
                               x.br_interface_inbox_full;
                               br_interface_inbox_msgs = (a x) })) (fun _ ->
                               u_msgs0) u_inbox
                           in
                           let u_inbox1 =
                             set (fun b -> b.br_interface_inbox_full)
                               (fun f ->
                               let i = fun r -> f r.br_interface_inbox_full in
                               (fun x -> { br_interface_inbox_full = 
                               (i x); br_interface_inbox_msgs =
                               x.br_interface_inbox_msgs })) (fun _ ->
                               u_full) u_inbox0
                           in
                           MonError.bind
                             (Barray.set u_inboxes (U64.of_u32 u_index)
                               u_inbox1) (fun u_inboxes0 ->
                             let u_ks0 =
                               set (fun b -> b.br_interface_kernel_inboxes)
                                 (fun f ->
                                 let a = fun r ->
                                   f r.br_interface_kernel_inboxes
                                 in
                                 (fun x -> { br_interface_kernel_current =
                                 x.br_interface_kernel_current;
                                 br_interface_kernel_deadline =
                                 x.br_interface_kernel_deadline;
                                 br_interface_kernel_next =
                                 x.br_interface_kernel_next;
                                 br_interface_kernel_next_quantum =
                                 x.br_interface_kernel_next_quantum;
                                 br_interface_kernel_mie =
                                 x.br_interface_kernel_mie;
                                 br_interface_kernel_zones =
                                 x.br_interface_kernel_zones;
                                 br_interface_kernel_intrp_handlers =
                                 x.br_interface_kernel_intrp_handlers;
                                 br_interface_kernel_inboxes = (a x);
                                 br_interface_kernel_ms =
                                 x.br_interface_kernel_ms })) (fun _ ->
                                 u_inboxes0) u_ks
                             in
                             let u_zones1 = u_ks0.br_interface_kernel_zones in
                             MonError.bind
                               (get u_zones1 (U64.of_u32 p_caller_id))
                               (fun u_caller1 ->
                               let u_regs1 = u_caller1.br_interface_zone_regs
                               in
                               MonError.bind
                                 (Barray.set u_regs1
                                   (U64.of_u32 coq_Br_ecall_A0) (Int.repr 1))
                                 (fun u_regs2 ->
                                 let u_caller2 =
                                   set (fun b -> b.br_interface_zone_regs)
                                     (fun f ->
                                     let a = fun r ->
                                       f r.br_interface_zone_regs
                                     in
                                     (fun x -> { br_interface_zone_id =
                                     x.br_interface_zone_id;
                                     br_interface_zone_regs = (a x);
                                     br_interface_zone_pmpcfg =
                                     x.br_interface_zone_pmpcfg;
                                     br_interface_zone_pmpaddr =
                                     x.br_interface_zone_pmpaddr;
                                     br_interface_zone_ustatus =
                                     x.br_interface_zone_ustatus;
                                     br_interface_zone_uepc =
                                     x.br_interface_zone_uepc;
                                     br_interface_zone_uie =
                                     x.br_interface_zone_uie;
                                     br_interface_zone_ucause =
                                     x.br_interface_zone_ucause;
                                     br_interface_zone_utval =
                                     x.br_interface_zone_utval;
                                     br_interface_zone_excpt_handlers =
                                     x.br_interface_zone_excpt_handlers;
                                     br_interface_zone_intrp_deleg =
                                     x.br_interface_zone_intrp_deleg }))
                                     (fun _ -> u_regs2) u_caller1
                                 in
                                 MonError.bind
                                   (Barray.set u_zones1
                                     (U64.of_u32 p_caller_id) u_caller2)
                                   (fun u_zones2 ->
                                   let u_ks1 =
                                     set (fun b ->
                                       b.br_interface_kernel_zones) (fun f ->
                                       let a = fun r ->
                                         f r.br_interface_kernel_zones
                                       in
                                       (fun x ->
                                       { br_interface_kernel_current =
                                       x.br_interface_kernel_current;
                                       br_interface_kernel_deadline =
                                       x.br_interface_kernel_deadline;
                                       br_interface_kernel_next =
                                       x.br_interface_kernel_next;
                                       br_interface_kernel_next_quantum =
                                       x.br_interface_kernel_next_quantum;
                                       br_interface_kernel_mie =
                                       x.br_interface_kernel_mie;
                                       br_interface_kernel_zones = (a x);
                                       br_interface_kernel_intrp_handlers =
                                       x.br_interface_kernel_intrp_handlers;
                                       br_interface_kernel_inboxes =
                                       x.br_interface_kernel_inboxes;
                                       br_interface_kernel_ms =
                                       x.br_interface_kernel_ms })) (fun _ ->
                                       u_zones2) u_ks0
                                   in
                                   MonError.ret u_ks1))))))))))))))

(** val coq_Br_ecall_ecall_recv :
    Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_ecall_ecall_recv p_caller_id p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 p_caller_id)) (fun u_caller ->
    let u_regs = u_caller.br_interface_zone_regs in
    MonError.bind (get u_regs (U64.of_u32 coq_Br_ecall_A0)) (fun u_sender ->
      MonError.bind
        (Barray.set u_regs (U64.of_u32 coq_Br_ecall_A0) (Int.repr 0))
        (fun u_regs0 ->
        let u_caller0 =
          set (fun b -> b.br_interface_zone_regs) (fun f ->
            let a = fun r -> f r.br_interface_zone_regs in
            (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
            br_interface_zone_regs = (a x); br_interface_zone_pmpcfg =
            x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
            x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
            x.br_interface_zone_ustatus; br_interface_zone_uepc =
            x.br_interface_zone_uepc; br_interface_zone_uie =
            x.br_interface_zone_uie; br_interface_zone_ucause =
            x.br_interface_zone_ucause; br_interface_zone_utval =
            x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
            x.br_interface_zone_excpt_handlers;
            br_interface_zone_intrp_deleg = x.br_interface_zone_intrp_deleg }))
            (fun _ -> u_regs0) u_caller
        in
        MonError.bind (Barray.set u_zones (U64.of_u32 p_caller_id) u_caller0)
          (fun u_zones0 ->
          let u_ks =
            set (fun b -> b.br_interface_kernel_zones) (fun f ->
              let a = fun r -> f r.br_interface_kernel_zones in
              (fun x -> { br_interface_kernel_current =
              x.br_interface_kernel_current; br_interface_kernel_deadline =
              x.br_interface_kernel_deadline; br_interface_kernel_next =
              x.br_interface_kernel_next; br_interface_kernel_next_quantum =
              x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
              x.br_interface_kernel_mie; br_interface_kernel_zones = 
              (a x); br_interface_kernel_intrp_handlers =
              x.br_interface_kernel_intrp_handlers;
              br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
              br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
              u_zones0) p_ks
          in
          if Int.cmpu Cge u_sender coq_Br_interface_N_ZONES
          then MonError.ret u_ks
          else let u_inboxes = u_ks.br_interface_kernel_inboxes in
               let u_index =
                 Int.add (Int.mul p_caller_id coq_Br_interface_N_ZONES)
                   u_sender
               in
               MonError.bind (get u_inboxes (U64.of_u32 u_index))
                 (fun u_inbox ->
                 let u_full = u_inbox.br_interface_inbox_full in
                 let u_msgs = u_inbox.br_interface_inbox_msgs in
                 let u_zones1 = u_ks.br_interface_kernel_zones in
                 MonError.bind (get u_zones1 (U64.of_u32 p_caller_id))
                   (fun u_caller1 ->
                   let u_regs1 = u_caller1.br_interface_zone_regs in
                   MonError.bind
                     (if Int.cmpu Cle u_full (Int.repr 0)
                      then MonError.ret u_regs1
                      else MonError.bind
                             (MonError.bind
                               (get u_msgs (U64.of_u32 (Int.repr 0)))
                               (fun b0 ->
                               Barray.set u_regs1
                                 (U64.of_u32 coq_Br_ecall_A1) b0))
                             (fun u_regs2 ->
                             MonError.bind
                               (MonError.bind
                                 (get u_msgs (U64.of_u32 (Int.repr 1)))
                                 (fun b1 ->
                                 Barray.set u_regs2
                                   (U64.of_u32 coq_Br_ecall_A2) b1))
                               (fun u_regs3 ->
                               MonError.bind
                                 (MonError.bind
                                   (get u_msgs
                                     (U64.of_u32 (Int.repr ((fun p->2*p) 1))))
                                   (fun b2 ->
                                   Barray.set u_regs3
                                     (U64.of_u32 coq_Br_ecall_A3) b2))
                                 (fun u_regs4 ->
                                 MonError.bind
                                   (get u_msgs
                                     (U64.of_u32
                                       (Int.repr ((fun p->1+2*p) 1))))
                                   (fun b3 ->
                                   Barray.set u_regs4
                                     (U64.of_u32 coq_Br_ecall_A4) b3)))))
                     (fun u_regs2 ->
                     MonError.bind
                       (Barray.set u_regs2 (U64.of_u32 coq_Br_ecall_A0)
                         (Int.repr 1)) (fun u_regs3 ->
                       let u_caller2 =
                         set (fun b -> b.br_interface_zone_regs) (fun f ->
                           let a = fun r -> f r.br_interface_zone_regs in
                           (fun x -> { br_interface_zone_id =
                           x.br_interface_zone_id; br_interface_zone_regs =
                           (a x); br_interface_zone_pmpcfg =
                           x.br_interface_zone_pmpcfg;
                           br_interface_zone_pmpaddr =
                           x.br_interface_zone_pmpaddr;
                           br_interface_zone_ustatus =
                           x.br_interface_zone_ustatus;
                           br_interface_zone_uepc = x.br_interface_zone_uepc;
                           br_interface_zone_uie = x.br_interface_zone_uie;
                           br_interface_zone_ucause =
                           x.br_interface_zone_ucause;
                           br_interface_zone_utval =
                           x.br_interface_zone_utval;
                           br_interface_zone_excpt_handlers =
                           x.br_interface_zone_excpt_handlers;
                           br_interface_zone_intrp_deleg =
                           x.br_interface_zone_intrp_deleg })) (fun _ ->
                           u_regs3) u_caller1
                       in
                       MonError.bind
                         (Barray.set u_zones1 (U64.of_u32 p_caller_id)
                           u_caller2) (fun u_zones2 ->
                         let u_ks0 =
                           set (fun b -> b.br_interface_kernel_zones)
                             (fun f ->
                             let a = fun r -> f r.br_interface_kernel_zones in
                             (fun x -> { br_interface_kernel_current =
                             x.br_interface_kernel_current;
                             br_interface_kernel_deadline =
                             x.br_interface_kernel_deadline;
                             br_interface_kernel_next =
                             x.br_interface_kernel_next;
                             br_interface_kernel_next_quantum =
                             x.br_interface_kernel_next_quantum;
                             br_interface_kernel_mie =
                             x.br_interface_kernel_mie;
                             br_interface_kernel_zones = (a x);
                             br_interface_kernel_intrp_handlers =
                             x.br_interface_kernel_intrp_handlers;
                             br_interface_kernel_inboxes =
                             x.br_interface_kernel_inboxes;
                             br_interface_kernel_ms =
                             x.br_interface_kernel_ms })) (fun _ -> u_zones2)
                             u_ks
                         in
                         let u_inbox0 =
                           set (fun b -> b.br_interface_inbox_msgs) (fun f ->
                             let a = fun r -> f r.br_interface_inbox_msgs in
                             (fun x -> { br_interface_inbox_full =
                             x.br_interface_inbox_full;
                             br_interface_inbox_msgs = (a x) })) (fun _ ->
                             u_msgs) u_inbox
                         in
                         let u_inbox1 =
                           set (fun b -> b.br_interface_inbox_full) (fun f ->
                             let i = fun r -> f r.br_interface_inbox_full in
                             (fun x -> { br_interface_inbox_full = (i x);
                             br_interface_inbox_msgs =
                             x.br_interface_inbox_msgs })) (fun _ -> u_full)
                             u_inbox0
                         in
                         MonError.bind
                           (Barray.set u_inboxes (U64.of_u32 u_index)
                             u_inbox1) (fun u_inboxes0 ->
                           let u_ks1 =
                             set (fun b -> b.br_interface_kernel_inboxes)
                               (fun f ->
                               let a = fun r ->
                                 f r.br_interface_kernel_inboxes
                               in
                               (fun x -> { br_interface_kernel_current =
                               x.br_interface_kernel_current;
                               br_interface_kernel_deadline =
                               x.br_interface_kernel_deadline;
                               br_interface_kernel_next =
                               x.br_interface_kernel_next;
                               br_interface_kernel_next_quantum =
                               x.br_interface_kernel_next_quantum;
                               br_interface_kernel_mie =
                               x.br_interface_kernel_mie;
                               br_interface_kernel_zones =
                               x.br_interface_kernel_zones;
                               br_interface_kernel_intrp_handlers =
                               x.br_interface_kernel_intrp_handlers;
                               br_interface_kernel_inboxes = (a x);
                               br_interface_kernel_ms =
                               x.br_interface_kernel_ms })) (fun _ ->
                               u_inboxes0) u_ks0
                           in
                           MonError.ret u_ks1))))))))))

(** val coq_Br_ecall_get_current_regA7 :
    coq_Br_interface_kernel -> Int.int res **)

let coq_Br_ecall_get_current_regA7 p_ks =
  let u_id = p_ks.br_interface_kernel_current in
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 u_id)) (fun u_caller ->
    get u_caller.br_interface_zone_regs (U64.of_u32 coq_Br_ecall_A7))

(** val coq_Br_ecall_handle_ecall_wrapped :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_ecall_handle_ecall_wrapped _ _ p_ks1 =
  let u_index = p_ks1.br_interface_kernel_current in
  MonError.bind (coq_Br_ecall_ecall_csrs_mie_KERNEL u_index p_ks1)
    (fun u_ks2 ->
    MonError.bind (coq_Br_ecall_get_current_regA7 u_ks2) (fun u_regA7 ->
      if Int.ltu u_regA7 coq_Br_ecall_N_ECALLS
      then if Int.eq u_regA7 coq_Br_ecall_ECL_EXCPT_VECT
           then MonError.bind (coq_Br_ecall_ecall_excpt_vect u_index u_ks2)
                  MonError.ret
           else if Int.eq u_regA7 coq_Br_ecall_ECL_INTRP_VECT
                then MonError.bind
                       (coq_Br_ecall_ecall_intrp_vect u_index u_ks2)
                       MonError.ret
                else if Int.eq u_regA7 coq_Br_ecall_ECL_CSRS_MIE
                     then MonError.bind
                            (coq_Br_ecall_ecall_csrs_mie_CALLER u_index u_ks2)
                            (fun u_ks22 ->
                            MonError.bind
                              (coq_Br_ecall_ecall_csrs_mie_KERNEL u_index
                                u_ks22) MonError.ret)
                     else if Int.eq u_regA7 coq_Br_ecall_ECL_CSRC_MIE
                          then MonError.bind
                                 (coq_Br_ecall_ecall_csrc_mie_CALLER u_index
                                   u_ks2) (fun u_ks22 ->
                                 MonError.bind
                                   (coq_Br_ecall_ecall_csrc_mie_KERNEL
                                     u_index u_ks22) MonError.ret)
                          else if Int.eq u_regA7 coq_Br_ecall_ECL_SEND
                               then MonError.bind
                                      (coq_Br_ecall_ecall_send u_index u_ks2)
                                      MonError.ret
                               else if Int.eq u_regA7 coq_Br_ecall_ECL_RECV
                                    then MonError.bind
                                           (coq_Br_ecall_ecall_recv u_index
                                             u_ks2) MonError.ret
                                    else MonError.ret u_ks2
      else MonError.ret u_ks2))

(** val coq_Br_ecall_handle_ecall :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_ecall_handle_ecall p_mcause p_mtval p_ks =
  let u_id = p_ks.br_interface_kernel_current in
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 u_id)) (fun u_caller ->
    let u_regs = u_caller.br_interface_zone_regs in
    MonError.bind
      (MonError.bind (get u_regs (U64.of_u32 coq_Br_interface_PC)) (fun b0 ->
        Barray.set u_regs (U64.of_u32 coq_Br_interface_PC)
          (Int.add b0 (Int.repr ((fun p->2*p) ((fun p->2*p) 1))))))
      (fun u_regs0 ->
      let u_caller0 =
        set (fun b -> b.br_interface_zone_regs) (fun f ->
          let a = fun r -> f r.br_interface_zone_regs in
          (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
          br_interface_zone_regs = (a x); br_interface_zone_pmpcfg =
          x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
          x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
          x.br_interface_zone_ustatus; br_interface_zone_uepc =
          x.br_interface_zone_uepc; br_interface_zone_uie =
          x.br_interface_zone_uie; br_interface_zone_ucause =
          x.br_interface_zone_ucause; br_interface_zone_utval =
          x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
          x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
          x.br_interface_zone_intrp_deleg })) (fun _ -> u_regs0) u_caller
      in
      MonError.bind (Barray.set u_zones (U64.of_u32 u_id) u_caller0)
        (fun u_zones0 ->
        let u_ks1 =
          set (fun b -> b.br_interface_kernel_zones) (fun f ->
            let a = fun r -> f r.br_interface_kernel_zones in
            (fun x -> { br_interface_kernel_current =
            x.br_interface_kernel_current; br_interface_kernel_deadline =
            x.br_interface_kernel_deadline; br_interface_kernel_next =
            x.br_interface_kernel_next; br_interface_kernel_next_quantum =
            x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
            x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
            br_interface_kernel_intrp_handlers =
            x.br_interface_kernel_intrp_handlers;
            br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
            br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
            u_zones0) p_ks
        in
        coq_Br_ecall_handle_ecall_wrapped p_mcause p_mtval u_ks1)))

(** val coq_Br_trap_EXCPT_ILLEGAL_INSTR : Int.int **)

let coq_Br_trap_EXCPT_ILLEGAL_INSTR =
  Int.repr ((fun p->2*p) 1)

(** val coq_Br_trap_EXCPT_ECALL_UMODE : Int.int **)

let coq_Br_trap_EXCPT_ECALL_UMODE =
  Int.repr ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))

(** val coq_Br_trap_INTRP_MACHINE_TIMER : Int.int **)

let coq_Br_trap_INTRP_MACHINE_TIMER =
  Int.repr ((fun p->1+2*p) ((fun p->1+2*p) 1))

(** val coq_Br_trap_OP_URET : Int.int **)

let coq_Br_trap_OP_URET =
  Int.repr ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->1+2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) ((fun p->2*p)
    ((fun p->2*p) ((fun p->2*p) ((fun p->2*p) 1)))))))))))))))))))))

(** val coq_Br_trap_set_ks_mie_and_bitwise :
    coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_trap_set_ks_mie_and_bitwise p_ks =
  let u_index = p_ks.br_interface_kernel_current in
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 u_index)) (fun u_zone ->
    let u_uie = u_zone.br_interface_zone_uie in
    let u_mie = p_ks.br_interface_kernel_mie in
    let u_res = coq_Br_interface_and_with_bitwise u_mie u_uie in
    MonError.ret
      (set (fun b -> b.br_interface_kernel_mie) (fun f ->
        let i = fun r -> f r.br_interface_kernel_mie in
        (fun x -> { br_interface_kernel_current =
        x.br_interface_kernel_current; br_interface_kernel_deadline =
        x.br_interface_kernel_deadline; br_interface_kernel_next =
        x.br_interface_kernel_next; br_interface_kernel_next_quantum =
        x.br_interface_kernel_next_quantum; br_interface_kernel_mie = 
        (i x); br_interface_kernel_zones = x.br_interface_kernel_zones;
        br_interface_kernel_intrp_handlers =
        x.br_interface_kernel_intrp_handlers; br_interface_kernel_inboxes =
        x.br_interface_kernel_inboxes; br_interface_kernel_ms =
        x.br_interface_kernel_ms })) (fun _ -> u_res) p_ks))

(** val coq_Br_trap_get_current_zone_handler :
    Int.int -> coq_Br_interface_kernel -> Int.int res **)

let coq_Br_trap_get_current_zone_handler p_mcause p_ks =
  let u_index = p_ks.br_interface_kernel_current in
  let u_zones = p_ks.br_interface_kernel_zones in
  MonError.bind (get u_zones (U64.of_u32 u_index)) (fun u_zone ->
    MonError.bind
      (get u_zone.br_interface_zone_excpt_handlers (U64.of_u32 p_mcause))
      MonError.ret)

(** val coq_Br_trap_get_i_handler_ZONE :
    Int.int -> coq_Br_interface_kernel -> Int.int res **)

let coq_Br_trap_get_i_handler_ZONE p_mcause p_ks =
  let u_i_handlers = p_ks.br_interface_kernel_intrp_handlers in
  MonError.bind
    (MonError.bind (coq_Br_interface_MCAUSE_INTRP p_mcause) (fun b0 ->
      get u_i_handlers (U64.of_u32 b0))) (fun u_i_handler ->
    MonError.ret u_i_handler.br_interface_intrp_handler_zone)

(** val coq_Br_trap_get_i_handler_HANDLER :
    Int.int -> coq_Br_interface_kernel -> Int.int res **)

let coq_Br_trap_get_i_handler_HANDLER p_mcause p_ks =
  let u_i_handlers = p_ks.br_interface_kernel_intrp_handlers in
  MonError.bind
    (MonError.bind (coq_Br_interface_MCAUSE_INTRP p_mcause) (fun b0 ->
      get u_i_handlers (U64.of_u32 b0))) (fun u_i_handler ->
    MonError.ret u_i_handler.br_interface_intrp_handler_handler)

(** val coq_Br_trap_wrapped_handle_user_intrp :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_trap_wrapped_handle_user_intrp p_mcause p_mtval p_ks =
  let u_i_handlers = p_ks.br_interface_kernel_intrp_handlers in
  MonError.bind
    (MonError.bind (coq_Br_interface_MCAUSE_INTRP p_mcause) (fun b0 ->
      get u_i_handlers (U64.of_u32 b0))) (fun u_i_handler ->
    let u_handler = u_i_handler.br_interface_intrp_handler_handler in
    if Int.cmpu Cgt u_handler (Int.repr 0)
    then let u_zones = p_ks.br_interface_kernel_zones in
         let u_index = u_i_handler.br_interface_intrp_handler_zone in
         MonError.bind (get u_zones (U64.of_u32 u_index)) (fun u_zone ->
           MonError.bind
             (MonError.bind
               (get u_zone.br_interface_zone_regs
                 (U64.of_u32 coq_Br_interface_PC)) (fun b1 ->
               MonError.ret
                 (set (fun b -> b.br_interface_zone_uepc) (fun f ->
                   let i = fun r -> f r.br_interface_zone_uepc in
                   (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                   br_interface_zone_regs = x.br_interface_zone_regs;
                   br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                   br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                   br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                   br_interface_zone_uepc = (i x); br_interface_zone_uie =
                   x.br_interface_zone_uie; br_interface_zone_ucause =
                   x.br_interface_zone_ucause; br_interface_zone_utval =
                   x.br_interface_zone_utval;
                   br_interface_zone_excpt_handlers =
                   x.br_interface_zone_excpt_handlers;
                   br_interface_zone_intrp_deleg =
                   x.br_interface_zone_intrp_deleg })) (fun _ -> b1) u_zone)))
             (fun u_zone0 ->
             let u_zone1 =
               set (fun b -> b.br_interface_zone_ucause) (fun f ->
                 let i = fun r -> f r.br_interface_zone_ucause in
                 (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                 br_interface_zone_regs = x.br_interface_zone_regs;
                 br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                 br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                 br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                 br_interface_zone_uepc = x.br_interface_zone_uepc;
                 br_interface_zone_uie = x.br_interface_zone_uie;
                 br_interface_zone_ucause = (i x); br_interface_zone_utval =
                 x.br_interface_zone_utval;
                 br_interface_zone_excpt_handlers =
                 x.br_interface_zone_excpt_handlers;
                 br_interface_zone_intrp_deleg =
                 x.br_interface_zone_intrp_deleg })) (fun _ -> p_mcause)
                 u_zone0
             in
             let u_zone2 =
               set (fun b -> b.br_interface_zone_utval) (fun f ->
                 let i = fun r -> f r.br_interface_zone_utval in
                 (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                 br_interface_zone_regs = x.br_interface_zone_regs;
                 br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                 br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                 br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                 br_interface_zone_uepc = x.br_interface_zone_uepc;
                 br_interface_zone_uie = x.br_interface_zone_uie;
                 br_interface_zone_ucause = x.br_interface_zone_ucause;
                 br_interface_zone_utval = (i x);
                 br_interface_zone_excpt_handlers =
                 x.br_interface_zone_excpt_handlers;
                 br_interface_zone_intrp_deleg =
                 x.br_interface_zone_intrp_deleg })) (fun _ -> p_mtval)
                 u_zone1
             in
             let u_regs = u_zone2.br_interface_zone_regs in
             MonError.bind
               (Barray.set u_regs (U64.of_u32 coq_Br_interface_PC) u_handler)
               (fun u_regs0 ->
               let u_zone3 =
                 set (fun b -> b.br_interface_zone_regs) (fun f ->
                   let a = fun r -> f r.br_interface_zone_regs in
                   (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                   br_interface_zone_regs = (a x); br_interface_zone_pmpcfg =
                   x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
                   x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
                   x.br_interface_zone_ustatus; br_interface_zone_uepc =
                   x.br_interface_zone_uepc; br_interface_zone_uie =
                   x.br_interface_zone_uie; br_interface_zone_ucause =
                   x.br_interface_zone_ucause; br_interface_zone_utval =
                   x.br_interface_zone_utval;
                   br_interface_zone_excpt_handlers =
                   x.br_interface_zone_excpt_handlers;
                   br_interface_zone_intrp_deleg =
                   x.br_interface_zone_intrp_deleg })) (fun _ -> u_regs0)
                   u_zone2
               in
               let u_zone4 =
                 set (fun b -> b.br_interface_zone_ustatus) (fun f ->
                   let i = fun r -> f r.br_interface_zone_ustatus in
                   (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                   br_interface_zone_regs = x.br_interface_zone_regs;
                   br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                   br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                   br_interface_zone_ustatus = (i x);
                   br_interface_zone_uepc = x.br_interface_zone_uepc;
                   br_interface_zone_uie = x.br_interface_zone_uie;
                   br_interface_zone_ucause = x.br_interface_zone_ucause;
                   br_interface_zone_utval = x.br_interface_zone_utval;
                   br_interface_zone_excpt_handlers =
                   x.br_interface_zone_excpt_handlers;
                   br_interface_zone_intrp_deleg =
                   x.br_interface_zone_intrp_deleg })) (fun _ ->
                   coq_Br_interface_USTATUS_PIE) u_zone3
               in
               let u_zone5 =
                 set (fun b -> b.br_interface_zone_intrp_deleg) (fun f ->
                   let i = fun r -> f r.br_interface_zone_intrp_deleg in
                   (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                   br_interface_zone_regs = x.br_interface_zone_regs;
                   br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                   br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                   br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                   br_interface_zone_uepc = x.br_interface_zone_uepc;
                   br_interface_zone_uie = x.br_interface_zone_uie;
                   br_interface_zone_ucause = x.br_interface_zone_ucause;
                   br_interface_zone_utval = x.br_interface_zone_utval;
                   br_interface_zone_excpt_handlers =
                   x.br_interface_zone_excpt_handlers;
                   br_interface_zone_intrp_deleg = (i x) })) (fun _ ->
                   Int.repr 1) u_zone4
               in
               MonError.bind
                 (Barray.set u_zones (U64.of_u32 u_index) u_zone5)
                 (fun u_zones0 ->
                 let u_ks =
                   set (fun b -> b.br_interface_kernel_zones) (fun f ->
                     let a = fun r -> f r.br_interface_kernel_zones in
                     (fun x -> { br_interface_kernel_current =
                     x.br_interface_kernel_current;
                     br_interface_kernel_deadline =
                     x.br_interface_kernel_deadline;
                     br_interface_kernel_next = x.br_interface_kernel_next;
                     br_interface_kernel_next_quantum =
                     x.br_interface_kernel_next_quantum;
                     br_interface_kernel_mie = x.br_interface_kernel_mie;
                     br_interface_kernel_zones = (a x);
                     br_interface_kernel_intrp_handlers =
                     x.br_interface_kernel_intrp_handlers;
                     br_interface_kernel_inboxes =
                     x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                     x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks
                 in
                 MonError.ret u_ks))))
    else MonError.ret p_ks)

(** val coq_Br_trap_handle_user_intrp :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_trap_handle_user_intrp p_mcause p_mtval p_ks =
  MonError.bind (coq_Br_trap_get_i_handler_HANDLER p_mcause p_ks)
    (fun u_handler ->
    MonError.bind (coq_Br_trap_get_i_handler_ZONE p_mcause p_ks)
      (fun u_zone ->
      let u_current = p_ks.br_interface_kernel_current in
      MonError.bind
        (coq_Br_trap_wrapped_handle_user_intrp p_mcause p_mtval p_ks)
        (fun u_ks1 ->
        MonError.bind (coq_Br_trap_set_ks_mie_and_bitwise u_ks1)
          (fun u_ks2 ->
          if (&&) (Int.cmpu Cgt u_handler (Int.repr 0))
               (Int.cmpu Cne u_zone u_current)
          then coq_Br_sched_sched_intrp u_ks2 u_zone
          else MonError.ret u_ks2))))

(** val coq_Br_trap_handle_user_excpt :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_trap_handle_user_excpt p_mcause p_mtval p_ks =
  MonError.bind (coq_Br_trap_get_current_zone_handler p_mcause p_ks)
    (fun u_handler ->
    if Int.cmpu Cgt u_handler (Int.repr 0)
    then let u_index = p_ks.br_interface_kernel_current in
         let u_zones = p_ks.br_interface_kernel_zones in
         MonError.bind (get u_zones (U64.of_u32 u_index)) (fun u_zone ->
           MonError.bind
             (MonError.bind
               (get u_zone.br_interface_zone_regs
                 (U64.of_u32 coq_Br_interface_PC)) (fun b0 ->
               MonError.ret
                 (set (fun b -> b.br_interface_zone_uepc) (fun f ->
                   let i = fun r -> f r.br_interface_zone_uepc in
                   (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                   br_interface_zone_regs = x.br_interface_zone_regs;
                   br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                   br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                   br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                   br_interface_zone_uepc = (i x); br_interface_zone_uie =
                   x.br_interface_zone_uie; br_interface_zone_ucause =
                   x.br_interface_zone_ucause; br_interface_zone_utval =
                   x.br_interface_zone_utval;
                   br_interface_zone_excpt_handlers =
                   x.br_interface_zone_excpt_handlers;
                   br_interface_zone_intrp_deleg =
                   x.br_interface_zone_intrp_deleg })) (fun _ -> b0) u_zone)))
             (fun u_zone0 ->
             let u_zone1 =
               set (fun b -> b.br_interface_zone_ucause) (fun f ->
                 let i = fun r -> f r.br_interface_zone_ucause in
                 (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                 br_interface_zone_regs = x.br_interface_zone_regs;
                 br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                 br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                 br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                 br_interface_zone_uepc = x.br_interface_zone_uepc;
                 br_interface_zone_uie = x.br_interface_zone_uie;
                 br_interface_zone_ucause = (i x); br_interface_zone_utval =
                 x.br_interface_zone_utval;
                 br_interface_zone_excpt_handlers =
                 x.br_interface_zone_excpt_handlers;
                 br_interface_zone_intrp_deleg =
                 x.br_interface_zone_intrp_deleg })) (fun _ -> p_mcause)
                 u_zone0
             in
             let u_zone2 =
               set (fun b -> b.br_interface_zone_utval) (fun f ->
                 let i = fun r -> f r.br_interface_zone_utval in
                 (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                 br_interface_zone_regs = x.br_interface_zone_regs;
                 br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
                 br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
                 br_interface_zone_ustatus = x.br_interface_zone_ustatus;
                 br_interface_zone_uepc = x.br_interface_zone_uepc;
                 br_interface_zone_uie = x.br_interface_zone_uie;
                 br_interface_zone_ucause = x.br_interface_zone_ucause;
                 br_interface_zone_utval = (i x);
                 br_interface_zone_excpt_handlers =
                 x.br_interface_zone_excpt_handlers;
                 br_interface_zone_intrp_deleg =
                 x.br_interface_zone_intrp_deleg })) (fun _ -> p_mtval)
                 u_zone1
             in
             let u_regs = u_zone2.br_interface_zone_regs in
             MonError.bind
               (Barray.set u_regs (U64.of_u32 coq_Br_interface_PC) u_handler)
               (fun u_regs0 ->
               let u_zone3 =
                 set (fun b -> b.br_interface_zone_regs) (fun f ->
                   let a = fun r -> f r.br_interface_zone_regs in
                   (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
                   br_interface_zone_regs = (a x); br_interface_zone_pmpcfg =
                   x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
                   x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
                   x.br_interface_zone_ustatus; br_interface_zone_uepc =
                   x.br_interface_zone_uepc; br_interface_zone_uie =
                   x.br_interface_zone_uie; br_interface_zone_ucause =
                   x.br_interface_zone_ucause; br_interface_zone_utval =
                   x.br_interface_zone_utval;
                   br_interface_zone_excpt_handlers =
                   x.br_interface_zone_excpt_handlers;
                   br_interface_zone_intrp_deleg =
                   x.br_interface_zone_intrp_deleg })) (fun _ -> u_regs0)
                   u_zone2
               in
               let u_ustatus = u_zone3.br_interface_zone_ustatus in
               let u_zone4 =
                 if Int.cmpu Cgt
                      (Int.coq_and u_ustatus coq_Br_interface_USTATUS_PIE)
                      (Int.repr 0)
                 then set (fun b -> b.br_interface_zone_ustatus) (fun f ->
                        let i = fun r -> f r.br_interface_zone_ustatus in
                        (fun x -> { br_interface_zone_id =
                        x.br_interface_zone_id; br_interface_zone_regs =
                        x.br_interface_zone_regs; br_interface_zone_pmpcfg =
                        x.br_interface_zone_pmpcfg;
                        br_interface_zone_pmpaddr =
                        x.br_interface_zone_pmpaddr;
                        br_interface_zone_ustatus = (i x);
                        br_interface_zone_uepc = x.br_interface_zone_uepc;
                        br_interface_zone_uie = x.br_interface_zone_uie;
                        br_interface_zone_ucause =
                        x.br_interface_zone_ucause; br_interface_zone_utval =
                        x.br_interface_zone_utval;
                        br_interface_zone_excpt_handlers =
                        x.br_interface_zone_excpt_handlers;
                        br_interface_zone_intrp_deleg =
                        x.br_interface_zone_intrp_deleg })) (fun _ ->
                        coq_Br_interface_USTATUS_PIE) u_zone3
                 else u_zone3
               in
               MonError.bind
                 (Barray.set u_zones (U64.of_u32 u_index) u_zone4)
                 (fun u_zones0 ->
                 let u_ks =
                   set (fun b -> b.br_interface_kernel_zones) (fun f ->
                     let a = fun r -> f r.br_interface_kernel_zones in
                     (fun x -> { br_interface_kernel_current =
                     x.br_interface_kernel_current;
                     br_interface_kernel_deadline =
                     x.br_interface_kernel_deadline;
                     br_interface_kernel_next = x.br_interface_kernel_next;
                     br_interface_kernel_next_quantum =
                     x.br_interface_kernel_next_quantum;
                     br_interface_kernel_mie = x.br_interface_kernel_mie;
                     br_interface_kernel_zones = (a x);
                     br_interface_kernel_intrp_handlers =
                     x.br_interface_kernel_intrp_handlers;
                     br_interface_kernel_inboxes =
                     x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                     x.br_interface_kernel_ms })) (fun _ -> u_zones0) p_ks
                 in
                 if Int.cmpu Cgt
                      (Int.coq_and u_ustatus coq_Br_interface_USTATUS_PIE)
                      (Int.repr 0)
                 then coq_Br_trap_set_ks_mie_and_bitwise u_ks
                 else MonError.ret u_ks))))
    else MonError.ret p_ks)

(** val coq_Br_trap_handle_uret :
    coq_Br_interface_kernel -> coq_Br_interface_kernel res **)

let coq_Br_trap_handle_uret p_ks =
  let u_zones = p_ks.br_interface_kernel_zones in
  let u_index = p_ks.br_interface_kernel_current in
  MonError.bind (get u_zones (U64.of_u32 u_index)) (fun u_zone ->
    let u_zuie = u_zone.br_interface_zone_uie in
    let u_ustatus = u_zone.br_interface_zone_ustatus in
    let u_regs = u_zone.br_interface_zone_regs in
    MonError.bind
      (Barray.set u_regs (U64.of_u32 coq_Br_interface_PC)
        u_zone.br_interface_zone_uepc) (fun u_regs0 ->
      let u_zone0 =
        set (fun b -> b.br_interface_zone_regs) (fun f ->
          let a = fun r -> f r.br_interface_zone_regs in
          (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
          br_interface_zone_regs = (a x); br_interface_zone_pmpcfg =
          x.br_interface_zone_pmpcfg; br_interface_zone_pmpaddr =
          x.br_interface_zone_pmpaddr; br_interface_zone_ustatus =
          x.br_interface_zone_ustatus; br_interface_zone_uepc =
          x.br_interface_zone_uepc; br_interface_zone_uie =
          x.br_interface_zone_uie; br_interface_zone_ucause =
          x.br_interface_zone_ucause; br_interface_zone_utval =
          x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
          x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
          x.br_interface_zone_intrp_deleg })) (fun _ -> u_regs0) u_zone
      in
      let u_zone1 =
        set (fun b -> b.br_interface_zone_ucause) (fun f ->
          let i = fun r -> f r.br_interface_zone_ucause in
          (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
          br_interface_zone_regs = x.br_interface_zone_regs;
          br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
          br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
          br_interface_zone_ustatus = x.br_interface_zone_ustatus;
          br_interface_zone_uepc = x.br_interface_zone_uepc;
          br_interface_zone_uie = x.br_interface_zone_uie;
          br_interface_zone_ucause = (i x); br_interface_zone_utval =
          x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
          x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
          x.br_interface_zone_intrp_deleg })) (fun _ -> Int.repr 0) u_zone0
      in
      let u_zone2 =
        set (fun b -> b.br_interface_zone_uepc) (fun f ->
          let i = fun r -> f r.br_interface_zone_uepc in
          (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
          br_interface_zone_regs = x.br_interface_zone_regs;
          br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
          br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
          br_interface_zone_ustatus = x.br_interface_zone_ustatus;
          br_interface_zone_uepc = (i x); br_interface_zone_uie =
          x.br_interface_zone_uie; br_interface_zone_ucause =
          x.br_interface_zone_ucause; br_interface_zone_utval =
          x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
          x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
          x.br_interface_zone_intrp_deleg })) (fun _ -> Int.repr 0) u_zone1
      in
      let u_zone3 =
        set (fun b -> b.br_interface_zone_utval) (fun f ->
          let i = fun r -> f r.br_interface_zone_utval in
          (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
          br_interface_zone_regs = x.br_interface_zone_regs;
          br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
          br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
          br_interface_zone_ustatus = x.br_interface_zone_ustatus;
          br_interface_zone_uepc = x.br_interface_zone_uepc;
          br_interface_zone_uie = x.br_interface_zone_uie;
          br_interface_zone_ucause = x.br_interface_zone_ucause;
          br_interface_zone_utval = (i x); br_interface_zone_excpt_handlers =
          x.br_interface_zone_excpt_handlers; br_interface_zone_intrp_deleg =
          x.br_interface_zone_intrp_deleg })) (fun _ -> Int.repr 0) u_zone2
      in
      let u_zone4 =
        if Int.cmpu Cgt (Int.coq_and u_ustatus coq_Br_interface_USTATUS_PIE)
             (Int.repr 0)
        then let u_res =
               Int.coq_or u_zone3.br_interface_zone_ustatus
                 coq_Br_interface_USTATUS_PIE
             in
             set (fun b -> b.br_interface_zone_ustatus) (fun f ->
               let i = fun r -> f r.br_interface_zone_ustatus in
               (fun x -> { br_interface_zone_id = x.br_interface_zone_id;
               br_interface_zone_regs = x.br_interface_zone_regs;
               br_interface_zone_pmpcfg = x.br_interface_zone_pmpcfg;
               br_interface_zone_pmpaddr = x.br_interface_zone_pmpaddr;
               br_interface_zone_ustatus = (i x); br_interface_zone_uepc =
               x.br_interface_zone_uepc; br_interface_zone_uie =
               x.br_interface_zone_uie; br_interface_zone_ucause =
               x.br_interface_zone_ucause; br_interface_zone_utval =
               x.br_interface_zone_utval; br_interface_zone_excpt_handlers =
               x.br_interface_zone_excpt_handlers;
               br_interface_zone_intrp_deleg =
               x.br_interface_zone_intrp_deleg })) (fun _ -> u_res) u_zone3
        else u_zone3
      in
      MonError.bind (Barray.set u_zones (U64.of_u32 u_index) u_zone4)
        (fun u_zones0 ->
        let u_ks =
          set (fun b -> b.br_interface_kernel_zones) (fun f ->
            let a = fun r -> f r.br_interface_kernel_zones in
            (fun x -> { br_interface_kernel_current =
            x.br_interface_kernel_current; br_interface_kernel_deadline =
            x.br_interface_kernel_deadline; br_interface_kernel_next =
            x.br_interface_kernel_next; br_interface_kernel_next_quantum =
            x.br_interface_kernel_next_quantum; br_interface_kernel_mie =
            x.br_interface_kernel_mie; br_interface_kernel_zones = (a x);
            br_interface_kernel_intrp_handlers =
            x.br_interface_kernel_intrp_handlers;
            br_interface_kernel_inboxes = x.br_interface_kernel_inboxes;
            br_interface_kernel_ms = x.br_interface_kernel_ms })) (fun _ ->
            u_zones0) p_ks
        in
        MonError.bind
          (if Int.cmpu Cgt
                (Int.coq_and u_ustatus coq_Br_interface_USTATUS_PIE)
                (Int.repr 0)
           then MonError.bind
                  (coq_Br_interface_binary_or u_ks.br_interface_kernel_mie
                    u_zuie) (fun u_new_mie ->
                  let u_ks0 =
                    set (fun b -> b.br_interface_kernel_mie) (fun f ->
                      let i = fun r -> f r.br_interface_kernel_mie in
                      (fun x -> { br_interface_kernel_current =
                      x.br_interface_kernel_current;
                      br_interface_kernel_deadline =
                      x.br_interface_kernel_deadline;
                      br_interface_kernel_next = x.br_interface_kernel_next;
                      br_interface_kernel_next_quantum =
                      x.br_interface_kernel_next_quantum;
                      br_interface_kernel_mie = (i x);
                      br_interface_kernel_zones =
                      x.br_interface_kernel_zones;
                      br_interface_kernel_intrp_handlers =
                      x.br_interface_kernel_intrp_handlers;
                      br_interface_kernel_inboxes =
                      x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                      x.br_interface_kernel_ms })) (fun _ -> u_new_mie) u_ks
                  in
                  MonError.ret u_ks0)
           else let u_ks0 =
                  set (fun b -> b.br_interface_kernel_mie) (fun f ->
                    let i = fun r -> f r.br_interface_kernel_mie in
                    (fun x -> { br_interface_kernel_current =
                    x.br_interface_kernel_current;
                    br_interface_kernel_deadline =
                    x.br_interface_kernel_deadline;
                    br_interface_kernel_next = x.br_interface_kernel_next;
                    br_interface_kernel_next_quantum =
                    x.br_interface_kernel_next_quantum;
                    br_interface_kernel_mie = (i x);
                    br_interface_kernel_zones = x.br_interface_kernel_zones;
                    br_interface_kernel_intrp_handlers =
                    x.br_interface_kernel_intrp_handlers;
                    br_interface_kernel_inboxes =
                    x.br_interface_kernel_inboxes; br_interface_kernel_ms =
                    x.br_interface_kernel_ms })) (fun _ ->
                    u_ks.br_interface_kernel_mie) u_ks
                in
                MonError.ret u_ks0) (fun _ ->
          MonError.bind
            (get u_ks.br_interface_kernel_zones (U64.of_u32 u_index))
            (fun b0 ->
            if Int.cmpu Cgt b0.br_interface_zone_intrp_deleg (Int.repr 0)
            then MonError.bind
                   (let b1 =
                      coq_Br_interface_read_mtime u_ks.br_interface_kernel_ms
                    in
                    MonError.ret
                      (set (fun b -> b.br_interface_kernel_deadline)
                        (fun f ->
                        let i = fun r -> f r.br_interface_kernel_deadline in
                        (fun x -> { br_interface_kernel_current =
                        x.br_interface_kernel_current;
                        br_interface_kernel_deadline = (i x);
                        br_interface_kernel_next =
                        x.br_interface_kernel_next;
                        br_interface_kernel_next_quantum =
                        x.br_interface_kernel_next_quantum;
                        br_interface_kernel_mie = x.br_interface_kernel_mie;
                        br_interface_kernel_zones =
                        x.br_interface_kernel_zones;
                        br_interface_kernel_intrp_handlers =
                        x.br_interface_kernel_intrp_handlers;
                        br_interface_kernel_inboxes =
                        x.br_interface_kernel_inboxes;
                        br_interface_kernel_ms = x.br_interface_kernel_ms }))
                        (fun _ -> b1) u_ks)) (fun u_ks0 ->
                   let u_zones1 = u_ks0.br_interface_kernel_zones in
                   let u_index0 = u_ks0.br_interface_kernel_current in
                   MonError.bind (get u_zones1 (U64.of_u32 u_index0))
                     (fun u_current ->
                     let u_current0 =
                       set (fun b -> b.br_interface_zone_intrp_deleg)
                         (fun f ->
                         let i = fun r -> f r.br_interface_zone_intrp_deleg in
                         (fun x -> { br_interface_zone_id =
                         x.br_interface_zone_id; br_interface_zone_regs =
                         x.br_interface_zone_regs; br_interface_zone_pmpcfg =
                         x.br_interface_zone_pmpcfg;
                         br_interface_zone_pmpaddr =
                         x.br_interface_zone_pmpaddr;
                         br_interface_zone_ustatus =
                         x.br_interface_zone_ustatus;
                         br_interface_zone_uepc = x.br_interface_zone_uepc;
                         br_interface_zone_uie = x.br_interface_zone_uie;
                         br_interface_zone_ucause =
                         x.br_interface_zone_ucause;
                         br_interface_zone_utval = x.br_interface_zone_utval;
                         br_interface_zone_excpt_handlers =
                         x.br_interface_zone_excpt_handlers;
                         br_interface_zone_intrp_deleg = (i x) })) (fun _ ->
                         Int.repr 0) u_current
                     in
                     MonError.bind
                       (Barray.set u_zones1 (U64.of_u32 u_index0) u_current0)
                       (fun u_zones2 ->
                       let u_ks1 =
                         set (fun b -> b.br_interface_kernel_zones) (fun f ->
                           let a = fun r -> f r.br_interface_kernel_zones in
                           (fun x -> { br_interface_kernel_current =
                           x.br_interface_kernel_current;
                           br_interface_kernel_deadline =
                           x.br_interface_kernel_deadline;
                           br_interface_kernel_next =
                           x.br_interface_kernel_next;
                           br_interface_kernel_next_quantum =
                           x.br_interface_kernel_next_quantum;
                           br_interface_kernel_mie =
                           x.br_interface_kernel_mie;
                           br_interface_kernel_zones = (a x);
                           br_interface_kernel_intrp_handlers =
                           x.br_interface_kernel_intrp_handlers;
                           br_interface_kernel_inboxes =
                           x.br_interface_kernel_inboxes;
                           br_interface_kernel_ms =
                           x.br_interface_kernel_ms })) (fun _ -> u_zones2)
                           u_ks0
                       in
                       coq_Br_sched_sched_yield u_ks1)))
            else MonError.ret u_ks)))))

(** val coq_Br_trap_handle_illegal_instruction :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_trap_handle_illegal_instruction _ p_mtval p_ks =
  if Int.eq p_mtval coq_Br_trap_OP_URET
  then coq_Br_trap_handle_uret p_ks
  else MonError.ret p_ks

(** val coq_Br_trap_handle_trap :
    Int.int -> Int.int -> coq_Br_interface_kernel -> coq_Br_interface_kernel
    res **)

let coq_Br_trap_handle_trap p_mcause p_mtval p_ks =
  let u_mcause_casted = I32.of_u32 p_mcause in
  MonError.bind
    (if Int.lt u_mcause_casted (Int.repr 0)
     then MonError.bind (coq_Br_interface_MCAUSE_INTRP p_mcause)
            (fun u_res ->
            if Int.eq u_res coq_Br_trap_INTRP_MACHINE_TIMER
            then MonError.ret p_ks
            else coq_Br_trap_handle_user_intrp p_mcause p_mtval p_ks)
     else if Int.eq p_mcause coq_Br_trap_EXCPT_ILLEGAL_INSTR
          then coq_Br_trap_handle_illegal_instruction p_mcause p_mtval p_ks
          else if Int.eq p_mcause coq_Br_trap_EXCPT_ECALL_UMODE
               then coq_Br_ecall_handle_ecall p_mcause p_mtval p_ks
               else coq_Br_trap_handle_user_excpt p_mcause p_mtval p_ks)
    coq_Br_sched_sched
