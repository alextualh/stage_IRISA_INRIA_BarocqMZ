
(** val make : int -> 'a1 -> 'a1 list **)

let rec make n v =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> [])
    (fun n' -> v :: (make n' v))
    n
