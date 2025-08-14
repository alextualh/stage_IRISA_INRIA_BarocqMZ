
(** val bind : 'a1 option -> ('a1 -> 'a2 option) -> 'a2 option **)

let bind f g =
  match f with
  | Some x -> g x
  | None -> None
