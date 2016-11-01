(* Run-length encoding of a list (direct solution). *)
(* Implement the so-called run-length encoding data compression method directly.
 * I.e. don't explicitly create the sublists containing the duplicates, as in
 * problem "Pack consecutive duplicates of list elements into sublists", but
 * only count them *)

(* my solution already works like that! *)

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

