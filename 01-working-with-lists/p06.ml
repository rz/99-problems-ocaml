(** Problem 6: Find out whether a list is a palindrome. *)

let rec reverse = function
  | [] -> []
  | hd :: tl -> reverse tl @ [hd]
;;

let rec is_palindrome l =
  reverse l = l
;;

