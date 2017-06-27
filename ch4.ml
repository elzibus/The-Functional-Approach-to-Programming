

(*
 * Hints for the Ocaml user, this was run on Ocaml 4.03.0
 * This should not be used to learn ocaml syntax, it's mostly about making the code in the book run in Ocaml.
 *)

(* 4.1 Exceptions *)

type 'a option = None | Some of 'a ;;

Some 12 ;;

(* 4.1.1 Examples of Exceptions *)

(* cannot have the following in this module if it is to be mod_use'd by another module
   2+(1/0)*3 ;;
   Exception: Division_by_zero.
 *)
  
(* cannot have the following in this module if it is to be mod_use'd by another module
   List.hd [] ;;
   Exception: Failure "hd".
 *)

(* 4.1.2 Defining Exceptions *)

exception Int_exception of int ;;
  
(* 4.1.3 Creating an Exceptional Value *)

Failure "oops" ;;  
  
(* cannot have the following in this module if it is to be mod_use'd by another module
   raise(Failure "oops") ;;
   Exception: Failure "oops".
 *)

let hd = function
    [] -> raise(Failure "hd")
  |(a::l) -> a ;;
  
(* hd [] ;;
   Exception: Failure "hd".
 *)

(* 4.1.4 Filtering Exceptional Values *)

try 1/0
with Division_by_zero -> 0 ;;

(* 4.1.5 Using Exceptions *)

let rec find p = function
    [] -> raise (Failure "find")
  |(a::l) -> if p a then a else find p l ;;
  
find (fun x -> x mod 2 = 0) [3;1;7;8;13;14] ;;

(* find (fun x -> x mod 2 = 0) [3;1;7;13] ;;
   Exception: Failure "find".
 *)

let failwith2 msg =
  raise (Failure msg) ;;

(* 4.2 Input and Output *)

(* 4.2.4 *)

type inttree = Leaf of int
	     | Node of inttree * inttree ;;
  
let rec print_inttree = function
  (Leaf n) -> print_int n
  |Node(t1,t2) -> print_string "(";
		  print_inttree t1;
		  print_string ",";
		  print_inttree t2;
		  print_string ")" ;;
		  
(* new_printer seems not to be in base Ocaml anymore *)

(* 4.2.5 Example: Interaction *)

let sentence f =
  print_string "Written score: ";
  let n1 = float_of_string(read_line()) in
  print_string "Oral score: ";
  let n2 = float_of_string(read_line()) in
  f(n1,n2) ;;

(* this does not work in the emacs tuareg repl, but
   works fine when run with "ocaml ch4.ml" on the command line *)
  
sentence (fun (x,y) -> let m = (x +. y)/.2.0 in
			 print_string (if m > 10.0 then "RECEIVED"
				       else "ADJOURNED") ;
			 print_newline()) ;;
	    
(* 4.3.1 Constructing Streams *)

#use "topfind" ;;

#camlp4o ;;
   
[< >] ;;
  
[< ''a'; ''b'; ''c' >] ;;
  
[ 'a'; 'b'; 'c' ] ;;

let n = 0 in
    [< 'n+1; 'n+2; 'n+3 >] ;;

let s1 = [< ''a'; ''b'; ''c' >] in
    [< ''0'; s1 >] ;;

let s1 = [< ''a'; ''b'; ''c' >] in
    let s2 = [< ''d'; ''e'; ''f' >] in
    [< s1; ''0'; s2 >] ;;

[ 1; (print_string "Hello\n"; 2); 3] ;;
  
[< '1; '(print_string "Hello\n"; 2); '3 >] ;;

let ints =
  let rec ints_from n = [< 'n; ints_from (n+1) >] in
  ints_from 0 ;;

Stream.of_channel ;;  

let read = parser [< 'x >] -> x ;;  

read ints ;;
  
read ints ;;
  
read ints ;;
  
