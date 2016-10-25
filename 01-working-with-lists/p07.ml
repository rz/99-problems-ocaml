(* Problem 7: Flatten a nested list structure. *)

(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list
;;

(* my original solution, which is clunky *)
(*
 *let rec flatten l =
 *  match l with
 *  | [] -> []
 *  | hd :: tl ->
 *      match hd with
 *      | One  a -> a :: flatten tl
 *      | Many a -> flatten a @ flatten tl
 *)

(* after reading the solution, i wrote this version which is easier to read *)
let rec flatten = function
  | [] -> []
  | One x :: tl -> x :: flatten tl
  | Many l :: tl -> flatten l @ flatten tl
;;
