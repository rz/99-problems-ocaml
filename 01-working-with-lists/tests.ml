open OUnit2


let test1 tests_ctxt = assert_equal (Some 3) (P01.last [1;2;3]);;
let test2 tests_ctxt = assert_equal None (P01.last []);;

let suite =
"suite">:::
  ["test1">:: test1;
   "test2">:: test2;]
;;

let () =
  run_test_tt_main suite
;;
