(* 
                         CS 51 Problem Set 1
                     Core Functional Programming
                             Spring 2018
 *)

val prob0_answer : string
val prob1a_answer : string
val prob1b_answer : string
val prob1c_answer : string

val reversed : int list -> bool
val merge : int list -> int list -> int list
val unzip : (int * int) list -> int list * int list
val variance : float list -> float option 
val few_divisors : int -> int -> bool
val concat_list : string -> string list -> string
val to_run_length : char list -> (int * char) list 
val from_run_length : (int * char) list -> char list 

val permutations : int list -> int list list

val minutes_spent_on_pset : unit -> int