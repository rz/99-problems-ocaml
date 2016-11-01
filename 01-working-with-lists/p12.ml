(* Decode a run-length encoded list. *)
(* Given a run-length code list generated as specified in the previous problem,
 * construct its uncompressed version *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let rec repeat n a =
  match n with
    | 0 -> []
    | 1 -> [a]
    | n -> a :: repeat (n - 1) a
;;
let rec decode = function
  | [] -> []
  | hd :: tl ->
      match hd with
      | One x -> x :: decode tl
      | Many (n, x) -> (repeat n x) @ decode tl
;;
