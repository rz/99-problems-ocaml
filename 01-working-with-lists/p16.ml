(* Drop every N'th element from a list. *)

open Core.Std

let rec drop l n =
  match l with
  | [] -> []
  | l -> let fst, lst = List.split_n l n in
    let fl = List.length fst in
    (if fl >= n then List.sub fst ~pos:0 ~len:(fl - 1) else fst) @ drop lst n
;;

