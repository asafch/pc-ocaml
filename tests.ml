#use "pc.ml";;

open PC;;

let a = pchar 'a'

let ai = pchar_ci 'A'

let b = pchar 'b'

let bi = pchar_ci 'B'

let a_then_b = and_then a b

let a_then_b_i = and_then ai bi

let hw = pstring "hello, world!"

let hw_ci = pstring_ci "HellO, WORld!"

let test_num = ref 1

let test result expected =
	let i = !test_num in
	if result = expected then
		Printf.printf "Test %3u passed" i
	else
		Printf.printf "Test %3u failed" i;
	test_num := !test_num + 1;;

(* 1 *)
test (run a "a") (Success('a', []));;
(* 2 *)
test (run a "b") Failure;;
(* 3 *)
test (run ai "A") (Success('A', []));;
(* 4 *)
test (run ai "a") (Success('a', []));;
(* 5 *)
test (run a_then_b "abc") (Success(('a', 'b'), ['c']));;
(* 6 *)
test (run a_then_b "Abc") Failure;;
(* 7 *)
test (run a_then_b_i "Abc") (Success(('A', 'b'), ['c']));;
(* 8 *)
test (run a_then_b_i "aBc") (Success(('a', 'B'), ['c']));;
(* 9 *)
test (run hw "hello, world!") (Success("hello, world!", []));;
(* 10 *)
test (run hw_ci "HELLO, WORLD!") (Success("HELLO, WORLD!", []));;
(* 11 *)
test (run hw "aBc") Failure;;
(* 12 *)
test (run (take_left a b) "ab") (Success('a', []));;
(* 13 *)
test (run (take_right a b) "ab") (Success('b', []));;
(* 14 *)
test (run (between a b a) "aba") (Success('b', []));;
(* 15 *)
test (run (maybe a) "ba") (Success(None, ['b'; 'a']));;
(* 16 *)
test (run (maybe a) "ab") (Success((Some('a')), ['b']));;
(* 17 *)
test (run (any_of [a; b]) "ac") (Success('a', ['c']));;
(* 18 *)
test (run (any_of [a; b]) "bc") (Success('b', ['c']));;
(* 19 *)
test (run (range 'A' 'Z') "AB") (Success('A', ['B']));;
(* 20 *)
test (run (range 'A' 'Z') "ZB") (Success('Z', ['B']));;
(* 21 *)
test (run (range 'A' 'Z') "TB") (Success('T', ['B']));;
(* 22 *)
test (run (range 'A' 'Z') "_BAD_RANGE_") Failure;;
(* 23 *)
test (run (star ai) "aAaaA") (Success (['a'; 'A'; 'a'; 'a'; 'A'], []));;
(* 24 *)
test (run (star ai) "baAaaA") (Success ([], ['b'; 'a'; 'A'; 'a'; 'a'; 'A']));;
(* 25 *)
test (run (plus ai) "aAaaA") (Success (['a'; 'A'; 'a'; 'a'; 'A'], []));;
(* 26 *)
test (run (plus ai) "taAaaA") Failure;;
(* 27 *)
test (run (plus ai) "atAaaA") (Success (['a'], ['t'; 'A'; 'a'; 'a'; 'A']));;