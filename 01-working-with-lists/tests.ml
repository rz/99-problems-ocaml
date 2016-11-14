open OUnit2


let tests_from_specs1 fxn_to_test specs =
  List.map
    (fun (label, arg, expected_result) -> (label>:: fun test_ctxt -> assert_equal (fxn_to_test arg) expected_result))
    specs

(* this is ugly, there has to be a way to say "apply this funciton to this tuple of arguments" *)
let tests_from_specs2 fxn_to_test specs =
  List.map
    (fun (label, (arg1, arg2), expected_result) ->
      (label>:: fun test_ctxt -> assert_equal (fxn_to_test arg1 arg2) expected_result))
    specs


let p01_tests = tests_from_specs1 P01.last [
  ("last is last", [1;2;3;4], Some 4);
  ("last of empty", [], None);
  ("last of one", [1], Some 1);
];;

let p02_tests = tests_from_specs1 P02.last_two [
  ("last 2 are last", [1;2;3;4;5;6], Some (5, 6));
  ("last 2 of 1", [1], None);
  ("last 2 of empty", [], None);
];;

let p03_tests = tests_from_specs2 P03.at [
  ("at 3 of 6", (3, [1;2;3;4;5;6]), Some 3);
  ("at 0 of 3", (0, [1;2;3]), None);
  ("at 1 of 1", (1, [1]), Some 1);
  ("at 1 of empty", (1, []), None);
];;

let p04_specs = [
  ("length of 3", [1;2;3], 3);
  ("length of 1", [1], 1);
  ("length of empty", [], 0);
];;

let p04_tests = (tests_from_specs1 P04.length p04_specs) @ (tests_from_specs1 P04.length_tail p04_specs)

let p05_tests = tests_from_specs1 P05.reverse [
  ("reverse empty", [], []);
  ("reverse 1", [1], [1]);
  ("reverse many", [1;2;3;4;5], [5;4;3;2;1]);
];;

let p06_tests = tests_from_specs1 P06.is_palindrome [
  ("empty is palindrome", [], true);
  ("single is palindrome", [1], true);
  ("double is palindrome if match", [1;1], true);
  ("dobule is not palindrome if diff", [1;2], false);
  ("long palindrome", [1;2;3;3;2;1], true);
  ("long not palindrome", [1;2;3;3;2;1;4], false);
]

open P07
let p07_tests = tests_from_specs1 P07.flatten [
  ("flatten empty", [], []);
  ("flatten ones", [ One "a"; One "b"; One "c"], ["a"; "b"; "c"]);
  ("flatten manys", [Many [One "a"; One "b"]; Many [One "c"; One "d"]], ["a"; "b"; "c"; "d"]);
  ("flatten mixed", [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ], ["a"; "b"; "c"; "d"; "e"]);
]

let p08_tests = tests_from_specs1 P08.compress [
  ("compress empty", [], []);
  ("compress single", ["a"], ["a"]);
  ("compress 2", ["a";"a"], ["a"]);
  ("compress 3", ["a"; "a"; "a"], ["a"]);
  ("compress 4", ["a";"a";"a";"a"], ["a"]);
  ("compress many",  ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"], ["a"; "b"; "c"; "a"; "d"; "e"]);
]

let p09_tests = tests_from_specs1 P09.pack [
  ("pack empty", [], []);
  ("pack single", [1], [[1]]);
  ("pack 3 2 3", [1;1;1;2;2;1;1;1], [[1;1;1];[2;2];[1;1;1]]);
  ("pack ex", [1;1;1;1;2;3;3;1;1;4;4;5;5;5;5], [[1;1;1;1];[2];[3;3];[1;1];[4;4];[5;5;5;5]]);
]

let p10_tests = tests_from_specs1 P10.encode [
  ("encode empty", [], []);
  ("encode ex", ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"], [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);
]

open P11
let p11_tests = tests_from_specs1 P11.encode [
  ("ecode empty", [], []);
  ("encode one", ["a"], [One "a"]);
  ("encode two", ["a"; "a"], [Many (2, "a")]);
  ("encode ex", ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"], [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);
]


open P12
let p12_tests = tests_from_specs1 P12.decode [
  ("decode empty", [], []);
  ("decode one", [One "a"], ["a"]);
  ("decode many", [Many(3, "a")], ["a"; "a"; "a"]);
  ("decode ex", [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")], ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
]

open P13
let p13_tests = tests_from_specs1 P13.encode [
  ("ecode empty", [], []);
  ("encode one", ["a"], [One "a"]);
  ("encode two", ["a"; "a"], [Many (2, "a")]);
  ("encode ex", ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"], [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);
]

let p14_tests = tests_from_specs1 P14.duplicate [
  ("duplicate empty", [], []);
  ("duplicate one", ["a"], ["a";"a"]);
  ("duplicate ex", ["a";"b";"c";"c";"d"], ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);
]

let p15_tests = tests_from_specs2 P15.replicate [
  ("replicate empty", ([], 5), []);
  ("replicate one 2", (["a"], 2), ["a";"a"]);
  ("replicate ex", (["a";"b";"c"], 3), ["a";"a";"a";"b";"b";"b";"c";"c";"c"]);
]

let p16_tests = tests_from_specs2 P16.drop [
  ("drop empty", ([], 3), []);
  ("drop ex", (["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"], 3), ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);
]

let suite =
  "Working with lists">:::
    p01_tests @ p02_tests @ p03_tests @ p04_tests @ p05_tests @ p06_tests @
    p07_tests @ p08_tests @ p09_tests @ p10_tests @ p11_tests @ p12_tests @
    p13_tests @ p14_tests @ p15_tests @ p16_tests
;;

let () =
  run_test_tt_main suite
;;

