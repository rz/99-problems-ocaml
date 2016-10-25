(* Problem 8: Eliminate consecutive duplicates of list elements. *)

let rec count_head_repeat l =
  let rec aux head count = function
    | [] -> count
    | [x] -> if x = head then count + 1 else count
    | hd :: tl -> if head = hd then aux head (count + 1) tl else count
  in match l with
    | [] -> 0
    | [x] -> 1
    | hd :: tl -> aux hd 0 l
;;

(* this is my original solution, but i'm having problems compiling when i try to run tests.
 * the reported error is that List.drop is unbound, not sure why.
 *
 *
 *let rec compress = function
 *  | [] -> []
 *  | hd :: tl -> hd :: (compress (List.drop (hd :: tl) (count_head_repeat (hd :: tl))))
 *;;
 *
 *)


(* since i can't use List.drop, i can write my own... *)
let rec drop l n =
  let rec aux count l =
    match l with
    | [] -> []
    | hd :: tl -> if count >= n then l else aux (count + 1) tl
  in aux 0 l
;;

let rec compress = function
  | [] -> []
  | hd :: tl -> hd :: (compress (drop (hd :: tl) (count_head_repeat (hd :: tl))))
;;

