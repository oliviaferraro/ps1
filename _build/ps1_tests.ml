(*
			 CS 51 Problem Set 1
		Core Functional Programming -- Testing
			     Spring 2017
 *)			     

open Ps1 ;;

(* 2a *)
let () = assert ((reversed [1;2;3]) = false);;
let () = assert ((reversed [3;2;1]) = true);;
let () = assert ((reversed [5;2;2;2;1;1]) = true);;
let () = assert ((reversed [1; 0; -1]) = true);;
let () = assert ((reversed [-1; 0; 1]) = false);;
let () = assert ((reversed []) = true);;

(* 2b *)
let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;

(* 2c *)
let () = assert ((unzip [(6,2);(2,4);(5,6)]) = ([6;2;5],[2;4;6]));;
let () = assert ((unzip []) = ([], []));;
let () = assert ((unzip [(-1, 3); (2, 4); (0, 4); (2, -3)]) = ([-1; 2; 0; 2], [3; 4; 4; -3]));;
let () = assert ((unzip [(0,0)]) = ([0], [0]));;

(* 2d *)
let () = assert ((variance [1.0; 2.0; 3.0; 4.0; 5.0]) = Some 2.5);;
let () = assert ((variance [1.0]) = None);;
let () = assert ((variance []) = None);;

(* 2e *)
let () = assert ((few_divisors 17 3) = true);;
let () = assert ((few_divisors 4 3) = false);;
let () = assert ((few_divisors 4 4) = true);;
let () = assert ((few_divisors 10 1) = false) ;;
let () = assert ((few_divisors 1 1) = false);;

(* 2f *)
let () = assert ((concat_list ", " ["first"; "second"; "third"]) = "first, second, third");;
let () = assert ((concat_list "..." ["Moo"; "Baa"; "Lalala"]) = "Moo...Baa...Lalala");;
let () = assert ((concat_list ", " []) = "");;
let () = assert ((concat_list ", " ["Moo"]) = "Moo");;
let () = assert ((concat_list "" ["hi"; "hello"; "yo"]) = "hihelloyo");;

(* 2g *)
let () = assert ((to_run_length ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd']) = [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]);;
let () = assert ((to_run_length ['x'; 'x'; 'y'; 'y'; 'x'; 'z'; 'y'; 'x']) = [(2, 'x'); (2, 'y'); (1, 'x'); (1, 'z'); (1, 'y'); (1, 'x')]);;
let () = assert ((to_run_length []) = []);;
let () = assert ((to_run_length ['a']) = [(1, 'a')]);;

let () = assert ((from_run_length [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]) = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd']);;
let () = assert ((from_run_length [(2, 'x'); (2, 'y'); (1, 'x'); (1, 'z'); (1, 'y'); (1, 'x')]) = ['x'; 'x'; 'y'; 'y'; 'x'; 'z'; 'y'; 'x']);;
let () = assert ((from_run_length []) = []);;
let () = assert ((from_run_length [(1, 'a')]) = ['a']);;
let () = assert ((from_run_length [(0, 'a')]) = []);;







