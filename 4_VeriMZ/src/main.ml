open Verimz.Kernel
open Init
open Print_state
open Printf

let run_kernel () =
  let rec run_kernel_rec orc st =
    printf "%s\n\n" (state_to_string st);
    match step orc st with
    | OK ((st', ev), orc') ->
        printf "~> %s\n\n" (event_to_string ev);
        ev :: run_kernel_rec orc' st'
    | Error _ -> []
  in
  run_kernel_rec (oracle_init ()) state_init

let () =
  let _ = Random.self_init () in
  print_endline "Running VeriMZ...\n";
  let _ = run_kernel () in
  print_endline "VeriMZ terminates !"
