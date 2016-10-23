(** Problem 5: Reverse a list. *)

let rec rev l =
  match l with
  | [] -> []
  | hd :: tl -> reverse tl @ [hd]
;;

