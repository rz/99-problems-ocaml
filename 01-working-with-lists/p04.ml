(** Problem 4: Find the number of elements of a list. *)

let rec length l =
  match l with
  | [] -> 0
  | hd :: tl -> 1 + length tl
;;

(** the tail-recursive version **)

let rec length_tail l =
  let rec aux acc l' =
    match l' with
    | [] -> acc
    | hd :: tl -> aux (acc + 1) tl
  in aux 0 l
;;

