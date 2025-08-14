open AST
open BinNums
open Ctypes

(** val tint : coq_type **)

let tint =
  Tint (I32, Signed, noattr)

(** val tuint : coq_type **)

let tuint =
  Tint (I32, Unsigned, noattr)

(** val tbool : coq_type **)

let tbool =
  Tint (IBool, Unsigned, noattr)

(** val tlong : coq_type **)

let tlong =
  Tlong (Signed, noattr)

(** val tulong : coq_type **)

let tulong =
  Tlong (Unsigned, noattr)

(** val tptr : coq_type -> coq_type **)

let tptr t =
  Tpointer (t, noattr)

(** val append_bit_pos : bool -> positive -> positive **)

let append_bit_pos b p =
  if b then Coq_xI p else Coq_xO p

(** val append_char_pos_default : char -> positive -> positive **)

let append_char_pos_default c p =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun b7 b6 b5 b4 b3 b2 b1 b0 -> Coq_xI (Coq_xI (Coq_xI (Coq_xI (Coq_xI
    (Coq_xI
    (append_bit_pos b0
      (append_bit_pos b1
        (append_bit_pos b2
          (append_bit_pos b3
            (append_bit_pos b4
              (append_bit_pos b5 (append_bit_pos b6 (append_bit_pos b7 p))))))))))))))
    c

(** val append_char_pos : char -> positive -> positive **)

let append_char_pos c p =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun b b0 b1 b2 b3 b4 b5 b6 ->
    if b
    then if b0
         then if b1
              then if b2
                   then if b3
                        then if b4
                             then append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
              else if b2
                   then if b3
                        then append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
         else if b1
              then if b2
                   then if b3
                        then append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
              else if b2
                   then if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
    else if b0
         then if b1
              then if b2
                   then if b3
                        then append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
              else if b2
                   then if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
         else if b1
              then if b2
                   then if b3
                        then append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xI (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xI (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
              else if b2
                   then if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xI p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xO p)))))
                                  else append_char_pos_default c p
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xI
                                              (Coq_xO (Coq_xI p)))))
                                  else append_char_pos_default c p
                   else if b3
                        then if b4
                             then if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xO (Coq_xO (Coq_xI
                                              (Coq_xI (Coq_xO p)))))
                                  else if b6
                                       then append_char_pos_default c p
                                       else Coq_xO (Coq_xO (Coq_xO (Coq_xO
                                              (Coq_xO (Coq_xO p)))))
                             else if b5
                                  then if b6
                                       then append_char_pos_default c p
                                       else Coq_xI (Coq_xI (Coq_xO (Coq_xO
                                              (Coq_xI (Coq_xI p)))))
                                  else append_char_pos_default c p
                        else append_char_pos_default c p)
    c

(** val ident_of_string : char list -> ident **)

let rec ident_of_string = function
| [] -> Coq_xH
| c::s0 -> append_char_pos c (ident_of_string s0)

(** val decode_8_bits : (positive -> char list) -> positive -> char list **)

let decode_8_bits rec0 = function
| Coq_xI q ->
  (match q with
   | Coq_xI q0 ->
     (match q0 with
      | Coq_xI q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\255'::(rec0 q6)
                     | Coq_xO q6 -> '\254'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\253'::(rec0 q6)
                     | Coq_xO q6 -> '\252'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\251'::(rec0 q6)
                     | Coq_xO q6 -> '\250'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\249'::(rec0 q6)
                     | Coq_xO q6 -> '\248'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\247'::(rec0 q6)
                     | Coq_xO q6 -> '\246'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\245'::(rec0 q6)
                     | Coq_xO q6 -> '\244'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\243'::(rec0 q6)
                     | Coq_xO q6 -> '\242'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\241'::(rec0 q6)
                     | Coq_xO q6 -> '\240'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\239'::(rec0 q6)
                     | Coq_xO q6 -> '\238'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\237'::(rec0 q6)
                     | Coq_xO q6 -> '\236'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\235'::(rec0 q6)
                     | Coq_xO q6 -> '\234'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\233'::(rec0 q6)
                     | Coq_xO q6 -> '\232'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\231'::(rec0 q6)
                     | Coq_xO q6 -> '\230'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\229'::(rec0 q6)
                     | Coq_xO q6 -> '\228'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\227'::(rec0 q6)
                     | Coq_xO q6 -> '\226'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\225'::(rec0 q6)
                     | Coq_xO q6 -> '\224'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\223'::(rec0 q6)
                     | Coq_xO q6 -> '\222'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\221'::(rec0 q6)
                     | Coq_xO q6 -> '\220'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\219'::(rec0 q6)
                     | Coq_xO q6 -> '\218'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\217'::(rec0 q6)
                     | Coq_xO q6 -> '\216'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\215'::(rec0 q6)
                     | Coq_xO q6 -> '\214'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\213'::(rec0 q6)
                     | Coq_xO q6 -> '\212'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\211'::(rec0 q6)
                     | Coq_xO q6 -> '\210'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\209'::(rec0 q6)
                     | Coq_xO q6 -> '\208'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\207'::(rec0 q6)
                     | Coq_xO q6 -> '\206'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\205'::(rec0 q6)
                     | Coq_xO q6 -> '\204'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\203'::(rec0 q6)
                     | Coq_xO q6 -> '\202'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\201'::(rec0 q6)
                     | Coq_xO q6 -> '\200'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\199'::(rec0 q6)
                     | Coq_xO q6 -> '\198'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\197'::(rec0 q6)
                     | Coq_xO q6 -> '\196'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\195'::(rec0 q6)
                     | Coq_xO q6 -> '\194'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\193'::(rec0 q6)
                     | Coq_xO q6 -> '\192'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xO q0 ->
     (match q0 with
      | Coq_xI q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\191'::(rec0 q6)
                     | Coq_xO q6 -> '\190'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\189'::(rec0 q6)
                     | Coq_xO q6 -> '\188'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\187'::(rec0 q6)
                     | Coq_xO q6 -> '\186'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\185'::(rec0 q6)
                     | Coq_xO q6 -> '\184'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\183'::(rec0 q6)
                     | Coq_xO q6 -> '\182'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\181'::(rec0 q6)
                     | Coq_xO q6 -> '\180'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\179'::(rec0 q6)
                     | Coq_xO q6 -> '\178'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\177'::(rec0 q6)
                     | Coq_xO q6 -> '\176'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\175'::(rec0 q6)
                     | Coq_xO q6 -> '\174'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\173'::(rec0 q6)
                     | Coq_xO q6 -> '\172'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\171'::(rec0 q6)
                     | Coq_xO q6 -> '\170'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\169'::(rec0 q6)
                     | Coq_xO q6 -> '\168'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\167'::(rec0 q6)
                     | Coq_xO q6 -> '\166'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\165'::(rec0 q6)
                     | Coq_xO q6 -> '\164'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\163'::(rec0 q6)
                     | Coq_xO q6 -> '\162'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\161'::(rec0 q6)
                     | Coq_xO q6 -> '\160'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\159'::(rec0 q6)
                     | Coq_xO q6 -> '\158'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\157'::(rec0 q6)
                     | Coq_xO q6 -> '\156'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\155'::(rec0 q6)
                     | Coq_xO q6 -> '\154'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\153'::(rec0 q6)
                     | Coq_xO q6 -> '\152'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\151'::(rec0 q6)
                     | Coq_xO q6 -> '\150'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\149'::(rec0 q6)
                     | Coq_xO q6 -> '\148'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\147'::(rec0 q6)
                     | Coq_xO q6 -> '\146'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\145'::(rec0 q6)
                     | Coq_xO q6 -> '\144'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\143'::(rec0 q6)
                     | Coq_xO q6 -> '\142'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\141'::(rec0 q6)
                     | Coq_xO q6 -> '\140'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\139'::(rec0 q6)
                     | Coq_xO q6 -> '\138'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\137'::(rec0 q6)
                     | Coq_xO q6 -> '\136'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\135'::(rec0 q6)
                     | Coq_xO q6 -> '\134'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\133'::(rec0 q6)
                     | Coq_xO q6 -> '\132'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\131'::(rec0 q6)
                     | Coq_xO q6 -> '\130'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\129'::(rec0 q6)
                     | Coq_xO q6 -> '\128'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xH -> [])
| Coq_xO q ->
  (match q with
   | Coq_xI q0 ->
     (match q0 with
      | Coq_xI q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\127'::(rec0 q6)
                     | Coq_xO q6 -> '~'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '}'::(rec0 q6)
                     | Coq_xO q6 -> '|'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '{'::(rec0 q6)
                     | Coq_xO q6 -> 'z'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'y'::(rec0 q6)
                     | Coq_xO q6 -> 'x'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'w'::(rec0 q6)
                     | Coq_xO q6 -> 'v'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'u'::(rec0 q6)
                     | Coq_xO q6 -> 't'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 's'::(rec0 q6)
                     | Coq_xO q6 -> 'r'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'q'::(rec0 q6)
                     | Coq_xO q6 -> 'p'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'o'::(rec0 q6)
                     | Coq_xO q6 -> 'n'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'm'::(rec0 q6)
                     | Coq_xO q6 -> 'l'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'k'::(rec0 q6)
                     | Coq_xO q6 -> 'j'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'i'::(rec0 q6)
                     | Coq_xO q6 -> 'h'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'g'::(rec0 q6)
                     | Coq_xO q6 -> 'f'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'e'::(rec0 q6)
                     | Coq_xO q6 -> 'd'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'c'::(rec0 q6)
                     | Coq_xO q6 -> 'b'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'a'::(rec0 q6)
                     | Coq_xO q6 -> '`'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '_'::(rec0 q6)
                     | Coq_xO q6 -> '^'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> ']'::(rec0 q6)
                     | Coq_xO q6 -> '\\'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '['::(rec0 q6)
                     | Coq_xO q6 -> 'Z'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'Y'::(rec0 q6)
                     | Coq_xO q6 -> 'X'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'W'::(rec0 q6)
                     | Coq_xO q6 -> 'V'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'U'::(rec0 q6)
                     | Coq_xO q6 -> 'T'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'S'::(rec0 q6)
                     | Coq_xO q6 -> 'R'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'Q'::(rec0 q6)
                     | Coq_xO q6 -> 'P'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'O'::(rec0 q6)
                     | Coq_xO q6 -> 'N'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'M'::(rec0 q6)
                     | Coq_xO q6 -> 'L'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'K'::(rec0 q6)
                     | Coq_xO q6 -> 'J'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'I'::(rec0 q6)
                     | Coq_xO q6 -> 'H'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'G'::(rec0 q6)
                     | Coq_xO q6 -> 'F'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'E'::(rec0 q6)
                     | Coq_xO q6 -> 'D'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'C'::(rec0 q6)
                     | Coq_xO q6 -> 'B'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> 'A'::(rec0 q6)
                     | Coq_xO q6 -> '@'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xO q0 ->
     (match q0 with
      | Coq_xI q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '?'::(rec0 q6)
                     | Coq_xO q6 -> '>'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '='::(rec0 q6)
                     | Coq_xO q6 -> '<'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> ';'::(rec0 q6)
                     | Coq_xO q6 -> ':'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '9'::(rec0 q6)
                     | Coq_xO q6 -> '8'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '7'::(rec0 q6)
                     | Coq_xO q6 -> '6'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '5'::(rec0 q6)
                     | Coq_xO q6 -> '4'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '3'::(rec0 q6)
                     | Coq_xO q6 -> '2'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '1'::(rec0 q6)
                     | Coq_xO q6 -> '0'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '/'::(rec0 q6)
                     | Coq_xO q6 -> '.'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '-'::(rec0 q6)
                     | Coq_xO q6 -> ','::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '+'::(rec0 q6)
                     | Coq_xO q6 -> '*'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> ')'::(rec0 q6)
                     | Coq_xO q6 -> '('::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\''::(rec0 q6)
                     | Coq_xO q6 -> '&'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '%'::(rec0 q6)
                     | Coq_xO q6 -> '$'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '#'::(rec0 q6)
                     | Coq_xO q6 -> '"'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '!'::(rec0 q6)
                     | Coq_xO q6 -> ' '::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO q1 ->
        (match q1 with
         | Coq_xI q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\031'::(rec0 q6)
                     | Coq_xO q6 -> '\030'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\029'::(rec0 q6)
                     | Coq_xO q6 -> '\028'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\027'::(rec0 q6)
                     | Coq_xO q6 -> '\026'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\025'::(rec0 q6)
                     | Coq_xO q6 -> '\024'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\023'::(rec0 q6)
                     | Coq_xO q6 -> '\022'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\021'::(rec0 q6)
                     | Coq_xO q6 -> '\020'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\019'::(rec0 q6)
                     | Coq_xO q6 -> '\018'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\017'::(rec0 q6)
                     | Coq_xO q6 -> '\016'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO q2 ->
           (match q2 with
            | Coq_xI q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\015'::(rec0 q6)
                     | Coq_xO q6 -> '\014'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\r'::(rec0 q6)
                     | Coq_xO q6 -> '\012'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\011'::(rec0 q6)
                     | Coq_xO q6 -> '\n'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\t'::(rec0 q6)
                     | Coq_xO q6 -> '\b'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xO q3 ->
              (match q3 with
               | Coq_xI q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\007'::(rec0 q6)
                     | Coq_xO q6 -> '\006'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\005'::(rec0 q6)
                     | Coq_xO q6 -> '\004'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xO q4 ->
                 (match q4 with
                  | Coq_xI q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\003'::(rec0 q6)
                     | Coq_xO q6 -> '\002'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xO q5 ->
                    (match q5 with
                     | Coq_xI q6 -> '\001'::(rec0 q6)
                     | Coq_xO q6 -> '\000'::(rec0 q6)
                     | Coq_xH -> [])
                  | Coq_xH -> [])
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xH -> [])
| Coq_xH -> []

(** val string_of_ident : positive -> char list **)

let rec string_of_ident = function
| Coq_xI p0 ->
  (match p0 with
   | Coq_xI p1 ->
     (match p1 with
      | Coq_xI p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> decode_8_bits string_of_ident p5
               | Coq_xO p5 -> 'v'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'L'::(string_of_ident p5)
               | Coq_xO p5 -> 'f'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'T'::(string_of_ident p5)
               | Coq_xO p5 -> 'n'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'D'::(string_of_ident p5)
               | Coq_xO p5 -> '7'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'X'::(string_of_ident p5)
               | Coq_xO p5 -> 'r'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'H'::(string_of_ident p5)
               | Coq_xO p5 -> 'b'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'P'::(string_of_ident p5)
               | Coq_xO p5 -> 'j'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'z'::(string_of_ident p5)
               | Coq_xO p5 -> '3'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xO p1 ->
     (match p1 with
      | Coq_xI p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'Z'::(string_of_ident p5)
               | Coq_xO p5 -> 't'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'J'::(string_of_ident p5)
               | Coq_xO p5 -> 'd'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'R'::(string_of_ident p5)
               | Coq_xO p5 -> 'l'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'B'::(string_of_ident p5)
               | Coq_xO p5 -> '5'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'V'::(string_of_ident p5)
               | Coq_xO p5 -> 'p'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'F'::(string_of_ident p5)
               | Coq_xO p5 -> '9'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'N'::(string_of_ident p5)
               | Coq_xO p5 -> 'h'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'x'::(string_of_ident p5)
               | Coq_xO p5 -> '1'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xH -> [])
| Coq_xO p0 ->
  (match p0 with
   | Coq_xI p1 ->
     (match p1 with
      | Coq_xI p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> '_'::(string_of_ident p5)
               | Coq_xO p5 -> 'u'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'K'::(string_of_ident p5)
               | Coq_xO p5 -> 'e'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'S'::(string_of_ident p5)
               | Coq_xO p5 -> 'm'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'C'::(string_of_ident p5)
               | Coq_xO p5 -> '6'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'W'::(string_of_ident p5)
               | Coq_xO p5 -> 'q'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'G'::(string_of_ident p5)
               | Coq_xO p5 -> 'a'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'O'::(string_of_ident p5)
               | Coq_xO p5 -> 'i'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'y'::(string_of_ident p5)
               | Coq_xO p5 -> '2'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xO p1 ->
     (match p1 with
      | Coq_xI p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'Y'::(string_of_ident p5)
               | Coq_xO p5 -> 's'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'I'::(string_of_ident p5)
               | Coq_xO p5 -> 'c'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'Q'::(string_of_ident p5)
               | Coq_xO p5 -> 'k'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'A'::(string_of_ident p5)
               | Coq_xO p5 -> '4'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xO p2 ->
        (match p2 with
         | Coq_xI p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'U'::(string_of_ident p5)
               | Coq_xO p5 -> 'o'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'E'::(string_of_ident p5)
               | Coq_xO p5 -> '8'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xO p3 ->
           (match p3 with
            | Coq_xI p4 ->
              (match p4 with
               | Coq_xI p5 -> 'M'::(string_of_ident p5)
               | Coq_xO p5 -> 'g'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xO p4 ->
              (match p4 with
               | Coq_xI p5 -> 'w'::(string_of_ident p5)
               | Coq_xO p5 -> '0'::(string_of_ident p5)
               | Coq_xH -> [])
            | Coq_xH -> [])
         | Coq_xH -> [])
      | Coq_xH -> [])
   | Coq_xH -> [])
| Coq_xH -> []
