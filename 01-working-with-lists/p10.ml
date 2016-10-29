(* Run-length encoding of a list. *)

let rec encode = function
  | [] -> []
  | [x] -> [(1, x)]
  | x :: tl -> let enctl = encode tl in
    match enctl with
    | [] -> [(1, x)]
    | (n, y) :: etl -> if x = y then (n + 1, y) :: etl else (1, x) :: enctl
;;

