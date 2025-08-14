open Barocqmzutils
open Barray
open Errors
open Integers
open List0

type timer_event = Int64.int

type interrupt_event =
| IevMachineExternal
| IevMachineSoftware

type zone_event =
| ZevTimerUpdate of timer_event
| ZevInterrupt of interrupt_event * timer_event

type oracle = { kevents : timer_event list; zevents : zone_event list list }

(** val hd_res : 'a1 list -> 'a1 res **)

let hd_res = function
| [] -> Error []
| x :: _ -> OK x

(** val call_koracle : oracle -> (timer_event * oracle) res **)

let call_koracle orc =
  let kevts = orc.kevents in
  bind (hd_res kevts) (fun hd -> OK (hd, { kevents = (tl kevts); zevents =
    orc.zevents }))

(** val call_zoracle : oracle -> Int.int -> (zone_event * oracle) res **)

let call_zoracle orc n_int =
  let n = int_to_int64 n_int in
  bind (get orc.zevents n) (fun zevts ->
    bind (set orc.zevents n (tl zevts)) (fun zevents' ->
      bind (hd_res zevts) (fun hd -> OK (hd, { kevents = orc.kevents;
        zevents = zevents' }))))
