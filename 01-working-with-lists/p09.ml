(* Pack consecutive duplicates of list elements into sublists. *)

(*
 *main idea: call pack on the tail, then combine
 *
 *eg assuming that pack 1 1 2 2 returns [1 1] [2 2]
 *
 *we should be able to pack 1 1 1 2 2 by calling pack on the tail
 *and combining:
 *
 *packed tail = [1 1] [2 2]
 *
 *pack 1 1 1 2 2 = prepend 1 to the head of packed tail
 *)

let rec pack = function
  | [] -> []
  | [x] -> [[x]]
  | a :: tl -> let packed = pack tl in
    match packed with
    | [] -> [[a]]
    | phd :: ptl ->
      match phd with
      | [] -> [[a]]
      | b :: pptl -> if a = b then [[a; b] @ pptl] @ ptl else [[a]] @ packed
;;

(* i would like to write it this way, but i don't understand why it doesn't work.
 * i'm not sure how pattern matching on nested lists works
 *
 *
 *let rec pack = function
 *  | [] -> []
 *  | [x] -> [[x]]
 *  | a :: tl -> let packed = pack tl in
 *    match packed with
 *    | [] -> [[a]]
 *    | [b :: phdtl] :: ptl -> if a = b then [a :: b :: phdtl] @ ptl else [a] :: packed
 *;;
 *)

