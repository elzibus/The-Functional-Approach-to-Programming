
(*
 * Hints for the Ocaml user, this was run on Ocaml 4.03.0
 * This should not be used to learn ocaml syntax, it's mostly about making the code in the book run in Ocaml.
 *)

(* 2.1 Record or Named Cartesian Products *)

let add_complex (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2) ;;

type complex = { re_part: float; im_part: float } ;;
  
let cx1 = { re_part=1.; im_part=0. } ;;
  
let cxi = { re_part=0.; im_part=1. } ;;

cx1.re_part ;;

fun { re_part = r1; im_part = i1} {re_part = r2; im_part = i2}
    -> {re_part = r1 +. r2; im_part = i1 +. i2 } ;;

fun c1 c2 -> {re_part = c1.re_part +. c2.re_part;
	      im_part = c1.im_part +. c2.im_part} ;;

type planar_point = { xcoord: float; ycoord: float } ;;

type circle = {center: planar_point; radius: float } ;;

type triangle = { ptA:planar_point;
		  ptB:planar_point;
		  ptC:planar_point} ;;
		  
(* 2.2 Sums with Constructors *)

(* 2.2.1 Constant Constructors *)

type suit = Club | Diamond | Heart | Spade ;;
  
Club ;;

function
  Club -> 1
| Diamond -> 2
| Heart -> 3
| Spade -> 4 ;;
  
(function Club -> 1 | Diamond -> 2 | Heart -> 3 | Spade -> 4) Diamond ;;

type bool = true | false ;;

(* 2.2.2 Constructors with Arguments *)

type num = Int of int | Float of float ;;
  
Int 3;;

Float 4.0 ;;

let add_num = function
    (Int m, Int n) -> Int ( m + n )
  | (Int m, Float n) -> Float( float_of_int m +. n)
  | (Float m, Int n) -> Float( m +. float_of_int n)
  | (Float m, Float n) -> Float ( m +. n ) ;;

(* 2.2.3 Types with Only One Constructor and Abbreviations *)

type angle = Angle of float ;;

(* ! no "==" in Ocaml for type abbreviations *)
  
type intpair = int * int ;;
  
type intpair = Intpair of int * int ;;

(* 2.2.4 Recursive Types *)

type inttree = Leaf of int
	     | Node of inttree * inttree ;;
  
Node(Leaf 3, Node (Leaf 4, Leaf 5)) ;;

let rec total = function
    Leaf n -> n
  | Node (t1, t2) -> total t1 + total t2 ;;

Node(Leaf 3, Node(Leaf 4, Leaf 5)) ;;

type exp = Constant of int
	 | Variable of string
	 | Addition of exp * exp
	 | Multiplication of exp * exp ;;
  

let rec eval env expression =
  match expression with
    Constant n -> n
  | Variable x -> env x
  | Addition (e1, e2) -> eval env e1 + eval env e2
  | Multiplication (e1, e2) -> (eval env e1) * (eval env e2) ;;
  
let rec deriv var expression =
  match expression with
    Constant n -> Constant 0
  | Variable x -> if x=var then Constant 1 else Constant 0
  | Addition (e1, e2) -> Addition( deriv var e1, deriv var e2)
  | Multiplication (e1, e2) -> Addition( Multiplication (e1, deriv var e2),
					 Multiplication (deriv var e1, e2)) ;;

(* some example calls *)
  
deriv "x" (Constant 5) ;;
deriv "x" (Addition(Variable "x", Variable "y")) ;;
deriv "x" (Addition(Constant 5, Multiplication( Variable "x", Variable "x"))) ;;
  
(* 2.2.5 Polymorphic Types *)

type 'a tree = Leaf of 'a
	     | Node of 'a tree * 'a tree ;;
  
Node(Leaf 3, Node(Leaf 4, Leaf 5)) ;;
  
type ('a, 'b) dict_entry = {content: 'a; key: 'b} ;;
  
{content= 1; key= "my key"} ;;
    
type ('a, 'b) dictionary = ('a, 'b) dict_entry list ;;
  
(* 2.2.6 Lists *)

type 'a list = Nil | Cons of 'a * 'a list ;;
  
Cons (2, Cons (3, Nil)) ;;

3 :: [] ;;

[3] ;;

[[1;2];[3;4]] ;;
  
[1; 1+1] @ [2+1;2+2] ;;

(* 2.2.7 Recursive Definitions of Values *)

let rec x = "one" :: y and y = "two" :: x;;

(* take n elements from a list *)  
let take n l =
  let rec take_helper n l =
    if n = 0 then []
    else (List.hd l):: take_helper (n-1)
				  (List.tl l)
  in
  take_helper n l;;

take 5 x ;;

take 10 y ;;

let rec t = Node(Leaf "one", Node ( Leaf "two", t)) ;;
  
(* 2.2.8 Abstract Types *)

(* module code not typed here.. *)

(* 2.2.9 Concreate Syntax of Data Structures *)

 
