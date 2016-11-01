(* Duplicate the elements of a list. *)

let rec duplicate = function
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate tl
;;

