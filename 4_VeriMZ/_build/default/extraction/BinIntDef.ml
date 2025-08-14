open BinPos

module Z =
 struct
  (** val to_nat : Stdlib.Int.t -> int **)

  let to_nat z =
    (fun f0 fp fn z -> if z=0 then f0 () else if z>0 then fp z else fn (-z))
      (fun _ -> 0)
      (fun p -> Pos.to_nat p)
      (fun _ -> 0)
      z
 end
