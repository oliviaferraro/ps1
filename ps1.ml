(* 
                         CS 51 Problem Set 1
                     Core Functional Programming
                             Spring 2018
 *)

(*======================================================================
Problem 1: Dealing with types

........................................................................
For each of the expressions below (prob0, prob1a, etc.), figure out
the type of the expression. Then write the type in as a string as the
value for prob0_answer, prob1a_answer, etc. The first one, prob0, is
done for you.

  prob0        42
  prob1a       let greet y = "Hello " ^ y in greet "World!"
  prob1b       [Some 4; Some 2; None; Some 3]
  prob1c       ((None, Some 42.0), true)

......................................................................*)

let prob0_answer = "int" ;;

let prob1a_answer = "string" ;;

let prob1b_answer = "int option list" ;;

let prob1c_answer = "('a option * float option) * bool" ;;

(*......................................................................
There are several values defined below that do not type check. 

Explain in a comment above each corresponding value why the following
definitions will not type check, and then provide a fixed version of 
each definition as an OCaml value (outside of a comment). Your fix should
change the code minimally.

(Note that the variable names begin with underscore (_) to disable the
warning noting that the values are otherwise unused. You'll want to
leave the underscores in as well.)
......................................................................*)

(* Is a list of (string * int) not a (string * (int list)). Need parentheses to
differentiate.

let _prob1d : string * int list = [("CS", 51); ("CS", 50)] ;; 
*)
  
let _prob1d : (string * int) list = [("CS", 51); ("CS", 50)] ;;

(* Can't add a float (3.9) and an int (4). Change operator to float operator and
add decimals to existing ints to make eveything float.

let _prob1e : int =
  let add (x, y) = x + y in
  if add (4, 3.9) = 10 then 4 else 2 ;; 
*)

let _prob1e : float =
  let add (x, y) = x +. y in
  if add (4.0, 3.9) = 10.0 then 4.0 else 2.0 ;;

(* Must accept option data type (None) so add Some in front of all existing ints
to make it a (string * int option) list.

let _prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); 
   ("April", None); ("May", None); ("June", 1); 
   ("July", None); ("August", None); ("September", 3);
   ("October", 1); ("November", 2); ("December", 3)] ;;
*)

let _prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); 
   ("April", None); ("May", None); ("June", Some 1); 
   ("July", None); ("August", None); ("September", Some 3);
   ("October", Some 1); ("November", Some 2); ("December", Some 3)] ;;

(*======================================================================
Problem 2 - Writing functions

........................................................................
For each subproblem, you must implement a given function, providing
appropriate unit tests in the accompanying file pset1_tests.ml. You
are provided a high level description as well as a type signature of
the function you must implement. Keep in mind the CS51 style guide and
what you've learned so far about efficiency and elegance. You are
*not* allowed to use library functions (i.e., the List module) for
*this* problem unless you implement the functionality yourself.
......................................................................*)

(*......................................................................
Problem 2a: The function "reversed" takes a list of integers and
returns true if the list is in nonincreasing order. The empty list is
considered to be reversed in this sense. Consecutive elements of the
same value are allowed in a reversed list.

For example:

# reversed [1;2;3] ;;
- : bool = false
# reversed [3;2;1] ;;
- : bool = true
# reversed [5;2;2;2;1;1] ;;
- : bool = true

Here is its signature: 

  reversed : int list -> bool

Replace the line below with your own definition of "reversed".
......................................................................*)

let rec reversed (lst : int list) : bool =
  (* if list has none or one elements return true *)
  match lst with
  | [] -> true
  | [_] -> true
  (* else compare 1st and 2nd element of the list and if in decreasing order 
  call reversed on tail *)
  | h1 :: (h2 :: _tl as tl) -> if h1 >= h2 then reversed tl else false ;;

(*......................................................................
Problem 2b: The function "merge" takes two integer lists, each
*sorted* in increasing order, and returns a single merged list in
sorted order.  For example:

merge [1;3;5] [2;4;6] ;;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;2;5] [2;4;6] ;;
- : int list = [1; 2; 2; 4; 5; 6]
merge [1;3;5] [2;4;6;12] ;;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12] ;;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its signature:

  merge : int list -> int list -> int list

Replace the line below with your own definition of "merge".
......................................................................*)

let rec merge (lst1 : int list) (lst2 : int list) : int list = 
  (* if one of the lists has no elements return the other list *)
  match lst1, lst2 with
  | [], a -> a
  | a, [] -> a
  (* else compare heads to boths lists and cons to new list in ascending order, 
  calling merge on the remaining tail  *)
  | h1 :: tl1, h2 :: tl2 -> 
          if h1 < h2 then h1 :: merge tl1 (h2 :: tl2)
          else h2 :: merge tl2 (h1 :: tl1) ;;

(*......................................................................
Problem 2c: The function "unzip", given a list of integer pairs,
returns a pair of lists, the first of which contains each first
element of each pair, and the second of which contains each second
element.  The returned list should have elements in the order in which
they were provided. For example:

unzip [(6,2);(2,4);(5,6)] ;;
- : int list * int list = ([6;2;5],[2;4;6])

Here is its signature:

  unzip : (int * int) list -> int list * int list)

Replace the line below with your own definition of "unzip".
......................................................................*)

let rec unzip (lst : (int * int) list) : int list * int list = 
  (* if list is empty return empty tuple *)
  match lst with
  | [] -> ([],[])
  (* else cons first element of tuple with first element of following tuple 
  (calling merge) into first position of new tuple, same for second element 
  and second position of the tuple respectively *)
  | (x, y) :: tl -> let (left, right) = unzip tl in 
                      (x :: left, y :: right) ;;

(*......................................................................
Problem 2d: The function "variance" takes a float list and returns
None if the list has fewer than two elements. Otherwise, it should
return Some of the variance of the floats. Recall that the variance of
a sequence of numbers is given by the following equation:
                                                
        1/(n-1) * sum (x_i - m)^2

where n indicates the number of elements in the list, m is the
arithmetic mean of the list, and x_i is element in the ith index of
the list. If you want to compare your output with an online
calculator, make sure you find one that calculates the (unbiased)
sample variance.  For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0] ;;
- : float option = Some 2.5
variance [1.0] ;;
- : float option = None

Remember to use the floating point version of the arithmetic operators
when operating on floats (+., *., etc). The function "float" can
convert ("cast") an int to a float.

Here is its signature:

  variance : float list -> float option

Replace the line below with your own definition of "variance".
......................................................................*)

let variance (lst : float list) : float option =
  (* calculate the length of given list *)
  let rec length lst =
    match lst with
    | [] -> 0
    | _hd :: tl -> 1 + length tl
  in
  (* adds up all elements in a list *)
  let rec sum lst =
    match lst with
    | [] -> 0.
    | hd :: tl -> hd +. sum tl
  in
  (* squares each element of the list minus the mean of the whole list *)
  let rec sub_sq lst mean =
    match lst with
    | [] -> []
    | hd :: tl -> ((hd -. mean) *. (hd -. mean)) :: sub_sq tl mean
  in
  (* if list has < 2 elements return None *)
  match lst with
  | [] | [_] -> None
  (* else calculate mean using sum and length, pass mean and list into sub_sq 
  and divide output buy length - 1  *)
  | _ -> let mean = (sum lst /. float (length lst)) in
         Some (sum (sub_sq lst mean) /. float (length lst - 1)) ;;

(*......................................................................
Problem 2e: The function "few divisors" takes two integers, x and y, and
returns true if x has fewer than y divisors (including 1 and x). Note:
this is *not* the same as x having fewer divisors than y does. For
example: 

few_divisors 17 3 ;;
- : bool = true
few_divisors 4 3 ;;
- : bool = false
few_divisors 4 4 ;;
- : bool = true

Do not worry about zero or negative integers at all. We will not test
your code using zero or negative values for x and y. Do not consider
negative integers for divisors (i.e. -2 being a divisor for 4).

Here is its signature:

  few_divisors : int -> int -> bool 

Replace the line below with your own definition of "few_divisors".
......................................................................*)

let few_divisors (x : int) (y : int) : bool =
  let half = x / 2 in
    (* if divisor is less than half + 1 return count *)
    let rec helper divisor count =
      if divisor >= (half + 1) then count
      (* else check if divisor is a factor of x, if so run helper again with 
      divisor and count both incremented by 1*)
      else if x mod divisor = 0 then helper (divisor + 1) (count + 1)
      (* if not a factor call helper and increment divisor, NOT count *)
      else helper (divisor + 1) count
    in
  (* if final output of helper is less than y return true, else false *)
  y > (helper 1 1) ;;

(*......................................................................
Problem 2f: The function "concat_list" takes two arguments: sep, a
string, and lst, a string list. It returns one string with all the
elements of lst concatenated together but separated by the string
sep. For example:

concat_list ", " ["first"; "second"; "third"] ;;
- : string = "first, second, third"
concat_list "..." ["Moo"; "Baa"; "Lalala"] ;;
- : string = "Moo...Baa...Lalala"
concat_list ", " [] ;;
- : string = ""
concat_list ", " ["Moo"] ;;
- : string = "Moo"

Here is its signature:

  concat_list : string -> string list -> string

Replace the lines below with your own definition of "concat_list"
......................................................................*)

let rec concat_list (sep : string) (lst : string list) : string =
  (* if list is empty return empty string, if list has 1 element return string 
  containing that element*)
  match lst with
  | [] -> ""
  | hd :: [] -> hd
  (* else concatenate head of list onto separator onto concat_list sep tail *)
  | hd :: tl -> hd ^ sep ^ concat_list sep tl ;;

(*......................................................................
Problem 2g: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'] 

we can (sometimes) represent the same information more compactly as a
list of pairs like 

  [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]      . 

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times, followed by 3 occurrences of the character 'b',
followed by one more 'a', and finally 4 copies of 'd'.

Write a function "to_run_length" that converts a list of characters
into the run-length encoding, and then write a function
"from_run_length" that converts back. Writing both functions will make
it easier to test that you've gotten them right.

Here are their signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the lines below with your own definitions of "to_run_length"
and "from_run_length".

......................................................................*)

let rec to_run_length lst =
  (* checks if head of given list matches target char given, if so call counter 
  and increment index by one. else return tuple (index (# of 
  occurances of a char), target char0 and the original list *)
  let rec counter lst index target =
    match lst with
    | [] -> (index, target), []
    | hd :: tl -> if hd = target then counter tl (index + 1) target
                      else (index, target), lst
  in
  (* if list is empty return empy list *)
  match lst with
  | [] -> []
  (* else call counter using list, 0, and head of list as target char. cons 
  results onto new list of tuples *)
  | hd :: tl -> let nums, oldlst = counter lst 0 hd in
                    nums :: (to_run_length oldlst);;

let rec from_run_length lst =
  (* if x > 0 cons y char x number of times onto new list, return that list. 
  else return empty list *)
  let rec write x y =
    if x <> 0 then y :: (write (x - 1) y) else []
  in
  (* if list is empty return empty list *)
  match lst with
  | [] -> []
  (* else append results from write with recursive call of to_run_length tail *)
  | (x, y) :: tl -> (write x y) @ (from_run_length tl) ;;

(*======================================================================
Problem 3: Challenge problem: Permutations

........................................................................
The function "permutations" takes a list of integers and should
return a list containing every permutation of the list. For example:

  permutations [1; 2; 3] =
  - : int list list = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; 
  [3; 1; 2]; [3; 2; 1]]

It doesn't matter what order the permutations appear in the returned
list.  Note that if the input list is of length n, then the answer
should be of length n! (that is, the factorial of n).

Hint: One way to do this is to write an auxiliary function, interleave
: int -> int list -> int list list, that yields all interleavings of
its first argument into its second. For example:

  interleave 1 [2; 3] = 
  - : int list list = [ [1; 2; 3]; [2; 1; 3]; [2; 3; 1] ]

You may also use list module functions for this question and may find 
List.map and List.concat helpful. 

Here is the signature of permutations:

  permutations : int list -> int list list

Replace the line below with your own definition of "permutations".
......................................................................*)

let permutations = (fun _ -> failwith "permutations not implemented") ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 1200 ;;
