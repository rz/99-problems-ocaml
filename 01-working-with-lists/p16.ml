(* Drop every N'th element from a list. *)

(* my original solution, which kinda sux *)
(*
 *open Core.Std
 *
 *let rec drop l n =
 *  match l with
 *  | [] -> []
 *  | l -> let fst, lst = List.split_n l n in
 *    let fl = List.length fst in
 *    (if fl >= n then List.sub fst ~pos:0 ~len:(fl - 1) else fst) @ drop lst n
 *;;
 *)

(* this is the official solution, which is much better *)

let drop list n =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t in
  aux 1 list
;;

