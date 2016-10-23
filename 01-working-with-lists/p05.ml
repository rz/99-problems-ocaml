(** Problem 5: Reverse a list. *)

let rec reverse l =
  match l with
  | [] -> []
  | hd :: tl -> reverse tl @ [hd]
;;

