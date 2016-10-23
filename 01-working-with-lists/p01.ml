(** Problem 1: Write a function [last : 'a list -> 'a option] that returns the
    last element of a list. *)

(* i don't understand why this doesn't compile :-/ *)
(*
 *let rec last l = function
 *  | [] -> None
 *  | [hd] -> Some hd
 *  | hd :: tl -> last tl
 *;;
 *
 *)

let rec last l =
  match l with
  | [] -> None
  | [hd] -> Some hd
  | hd :: tl -> last tl
;;

