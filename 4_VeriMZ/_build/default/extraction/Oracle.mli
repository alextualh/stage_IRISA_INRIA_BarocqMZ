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

val hd_res : 'a1 list -> 'a1 res

val call_koracle : oracle -> (timer_event * oracle) res

val call_zoracle : oracle -> Int.int -> (zone_event * oracle) res
