(* Modified run-length encoding. *)
(* modify the solution from the previous problem so that if an element has no
 * duplicates it is simply copied into the resulting list. *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let rec encode = function
  | [] -> []
  | [x] -> [One x]
  | x :: tl -> let enctl = encode tl in
    match enctl with
    | [] -> [One x]
    | One y :: etl -> if x = y then Many(2, x) :: etl else One x :: enctl
    | Many(n, y) :: etl -> if x = y then Many(n + 1, x) :: etl else One x :: enctl
;;

