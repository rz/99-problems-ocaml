(** Problem 3: Find the [k]'th element of a list. *)

let rec at k l =
  match k, l with
  | _, [] -> None
  | 0, _ -> None
  | 1, hd :: tl -> Some hd
  | k, hd :: tl -> at (k - 1) tl
;;

