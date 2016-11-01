(* Replicate the elements of a list a given number of times. *)

let rec repeat n a =
  match n with
    | 0 -> []
    | 1 -> [a]
    | n -> a :: repeat (n - 1) a
;;

let rec replicate l n =
  match l with
  | [] -> []
  | hd :: tl -> repeat n hd @ replicate tl n
;;

