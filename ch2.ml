
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

(* you could redefine bool but I don't do it here as it creates issues if
 * this code is to be included from another file with mod_use..
 *
 *  type bool = true | false ;;
 *)
  
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

(* Note: renamed so that this file can be made into a module *)
  
type intpair_abbrev = int * int ;;
  
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

(* inttree_of_string is not given in the book so here is a version *)
  
#load "str.cma" ;;
    
(* we keep the string value and an index inside the string *)  
type stringp = { str: string ; ptr: int  } ;;
  
let rec skip_spaces strp =
  if strp.ptr > (String.length strp.str) -1 then strp
  else if strp.str.[strp.ptr] = ' ' || strp.str.[strp.ptr] = '\t' then skip_spaces {str = strp.str; ptr = strp.ptr + 1}
  else strp ;;
  
let peek_char strp =
  if strp.ptr > (String.length strp.str) -1 then failwith "peek_char is looking after the end of the string"
  else strp.str.[strp.ptr] ;;
  
let next_char strp = {str= strp.str; ptr=strp.ptr+1} ;;
  
let is_digit_char c = c >= '0' && c <= '9' ;;
  
let read_int strp =
  let rec read_int_helper strp res =
    if strp.ptr > (String.length strp.str) - 1 then (strp, Leaf (int_of_string res))
    else let c = peek_char strp in
	 if is_digit_char c then read_int_helper {str=strp.str; ptr=strp.ptr+1} (res ^ (String.make 1 c))
	 else  (strp, Leaf (int_of_string res))
  in
  read_int_helper strp "" ;;

let inttree_of_string str =   
  let rec ios_helper strp =
    let strp1 = skip_spaces strp in
    if peek_char strp1 = '(' then ( let strp2 = next_char strp1 in
				    let (strp3, t1) = ios_helper strp2 in
				    if peek_char strp3 = ',' then ( let strp4 = next_char strp3 in
								    let(strp5, t2) = ios_helper strp4 in
								    if peek_char strp5 = ')' then (next_char strp5, Node( t1, t2))
								    else failwith "Wrong syntax" )
				    else failwith "Wrong syntax" )
    else if is_digit_char (peek_char strp1) then read_int strp1
    else failwith "Wrong syntax" in
  snd (ios_helper {str=str;ptr=0}) ;;
  
inttree_of_string "2" ;;

inttree_of_string "(1,(2,3))" ;;

inttree_of_string "(1,(2,(3,(4,(5,(6,(7,8)))))))" ;;

let rec string_of_inttree t =
  match t with
    Leaf i -> string_of_int i
  | Node ( l1, l2 ) -> "(" ^ string_of_inttree l1 ^ "," ^ string_of_inttree l2 ^ ")" ;;

string_of_inttree (inttree_of_string "2") ;;

string_of_inttree (inttree_of_string "(1,(2,3))");;

string_of_inttree (inttree_of_string "(1,(2,(3,(4,(5,(6,(7,8)))))))");;

(* TODO: exp_of_string *)

let is_var_char c = not ( c = ',' || c = '(' || c = ')') ;;

let read_var conv strp =
  let rec rv_helper strp res =
    if strp.ptr > (String.length strp.str) - 1 then (strp, Leaf (conv res))
    else let c = peek_char strp in
	 if is_var_char c then rv_helper {str=strp.str; ptr=strp.ptr+1} (res ^ (String.make 1 c))
	 else  (strp, Leaf (conv res))
  in
  rv_helper strp "" ;;
  
let tree_of_string conv str =
  let rec tos_helper strp =
    let strp1 = skip_spaces strp in
    if peek_char strp1 = '(' then ( let strp2 = next_char strp1 in
				    let (strp3, t1) = tos_helper strp2 in
				    if peek_char strp3 = ',' then ( let strp4 = next_char strp3 in
								    let(strp5, t2) = tos_helper strp4 in
								    if peek_char strp5 = ')' then (next_char strp5, Node( t1, t2))
								    else failwith "Wrong syntax" )
				    else failwith "Wrong syntax" )
    else if is_var_char (peek_char strp1) then read_var conv strp1
    else failwith "Wrong syntax" in
  snd (tos_helper {str=str;ptr=0}) ;;

tree_of_string int_of_string "(2,(3,4))" ;;

tree_of_string (fun x-> x) "(2,(3,4))" ;;

(* 2.3 Lists *)

let rec length = function
    [] -> 0
  | (a::l) -> 1 + length l ;;
  
length [1;2;3] ;;

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | (a::l) -> a::append l l2 ;;
  
append [1;2;3] [4;5;6] ;;

let rec rev = function
    [] -> []
  | (a::l) -> append (rev l) [a] ;;
  
rev ["d";"c";"b";"a"] ;;
  
let rec sigma = function
    [] -> 0
  | (a::l) -> a + sigma l ;;
  
sigma [1;2;3] ;;
  
let rec pi = function
    [] -> 1
  | (a::l) -> a * pi l ;;
  
pi [1;2;3;4;5] ;;
  
let rec map f l =
  match l with
    [] -> []
  | (a::l) -> (f a)::(map f l) ;;
  
map (fun x -> 1 + x) [1;2;3] ;;

let rec flat = function
    [] -> []
  | (l::ll) -> append l (flat ll) ;;
  
flat [ [1;2]; [3;4;5] ] ;;

(* 2.3.1 General Functionals for Lists *)

let rec list_hom e f l =
  match l with
    [] -> e
  | (a::l) -> f a (list_hom e f l) ;;
  
let length = list_hom 0 (fun _ y -> 1 + y) ;;

length [1;2;3;4] ;;

let append l1 l2 = list_hom l2 (fun x l -> x::l) l1 ;;

append [1;2;3] [4;5;6;7;8] ;;

let rev = list_hom [] (fun e l -> append l [e]) ;;  

rev [1;2;3;4;5;6] ;;
  
let sigma = list_hom 0 ( + ) ;;

sigma [1;2;3] ;;

(* thanks for the fix github.com/fourchaux ! *)
let pi = list_hom 1 ( * ) ;;

pi [1;2;3;4;5] ;;

let map f = list_hom [] (fun e l -> f(e) :: l ) ;;
  
map (fun x -> x+2) [1;2;3] ;;

let flat = list_hom [] append ;;
  
flat [["a";"B"];["c";"d"]];;

let rec list_it f l e =
  match l with
    [] -> e
  | (a::l) -> f a (list_it f l e) ;;
  
let rec it_list f e l =
  match l with
    [] -> e
  | (a::l) -> it_list f (f e a) l ;;

(* 2.3.2 Partitioning and Sorting *)

let partition test l =
  let switch elem (l1, l2) =
    if test elem then (l1, elem::l2) else (elem::l1, l2)
  in list_it switch l ([],[]) ;;

partition ( fun x -> x > 2 ) [1;2;3;4] ;;  

let filter test l = snd ( partition test l ) ;;  

filter ( fun x -> x > 2 ) [1;2;3;4] ;;  

filter ( fun x -> (x mod 2  ) = 0) [2;3;5;8;9;12;15] ;;
  
let rec quicksort order l =
  match l with
    [] -> []
  | a::l -> let (l1,l2) = partition (order a) l in
	    (quicksort order l1) @ (a::(quicksort order l2)) ;;
  
quicksort (fun x y -> x < y) [1;3;2;1;4;3;2;3;4] ;;

quicksort ( < ) [6;3;9;1;2;7] ;;

quicksort ( > ) [6;3;9;1;2;7] ;;
  
quicksort ( < ) [0.25;0.125;0.1095;0.3] ;;
  
let rec insert order elem list =
  match list with
    [] -> [elem]
  | (a::l) -> if order elem a then elem::a::l
	      else a:: insert order elem l ;;

(* ! next function is fixed in the book errata *)
  
let sort order xs = list_it (insert order) xs [] ;;  

sort ( < ) [3;2;4;5;2;3;1;3] ;;  

(* Representing Sets by Lists *)

let rec member equiv e list =
  match list with
    [] -> false
  |(a::l) -> (equiv a e) || member equiv e l ;;

member ( = ) 1 [1;2;3] ;;

member ( = ) 10 [1;2;3] ;;

member ( = ) 1 [1;2;3;1] ;;

let rec rem_from_list equiv e list =
  match list with
    [] -> []
  | (a::l) -> if (equiv a e) then rem_from_list equiv e l
	      else a::rem_from_list equiv e l ;;
  
(* version from the book *)


let rec rem_from_list equiv e list =
  match list with
    [] -> []
  | (a::l) -> let l' = rem_from_list equiv e l in
	      if (equiv a e) then l'
	      else a::l' ;;
  
let rec make_set equiv list =
  match list with
    [] -> []
  | (a::l) -> a::make_set equiv (rem_from_list equiv a l) ;;
  
make_set ( = ) [1;2;1;2;3;2;1;3;4;2;5;6] ;;

let rec rem_from_set equiv e list =
  match list with
    [] -> []
  | (a::l) -> if (equiv a e) then l
	      else a::rem_from_set equiv e l ;;

let add_to_set equiv e l =
  if member equiv e l then l else e::l ;;

let rec union equiv (l1,l2) =
  list_it (add_to_set equiv) l1 l2 ;;

union ( = ) ([1;2;3], [1;2;3;4;5]) ;;

let inter equiv (l1, l2) =
  filter (fun x -> member equiv x l2) l1 ;;

inter ( = ) ([1;2;3], [1;3;4;5]) ;;

let rec member (order, equiv) e list =
  match list with
    [] -> false
  | (a::l) -> if order(a,e) then member (order, equiv) e l
	      else equiv(a,e) ;;

(* parens around each function in the pair must be used in the expressions below *)
  
member ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) 1 [1;2;3] ;;

member ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) 100 [1;2;3] ;;

(* new name as previous function is used in the exercises *)

let rec add_to_set2 (order,equiv) elem list =
  match list with
    [] -> [elem]
  | (a::l) -> if order(elem,a) then elem::a::l
	      else if equiv(elem,a) then a::l
	      else a::add_to_set2 (order, equiv) elem l ;;
		  
add_to_set2 ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) 'f' ['a';'g';'z'] ;;

add_to_set2 ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) 'g' ['a';'g';'z'] ;;

let rec inter (order, equiv) = function
    ([],_) -> []
  | (_,[]) -> []
  | ((a1::l1 as ll1), (a2::l2 as ll2)) ->
     if equiv(a1,a2) then a1::inter (order, equiv) (l1, l2)
     else if order(a1,a2) then inter (order, equiv) (l1, ll2)
     else inter (order, equiv) (ll1, l2) ;;

inter ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) (['g'], ['a';'g';'z']) ;;
  
inter ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) (['g';'h'], ['g';'h']) ;;

(* new name as previous function is used in the exercises *)
  
let rec union2 (order, equiv) = function
    ([], l2) -> l2
  | (l1, []) -> l1
  | ((a1::l1 as ll1), (a2::l2 as ll2)) ->
     if equiv(a1,a2) then a1::union2 (order, equiv) (l1, l2)
     else if order(a1,a2) then a1::union2 (order, equiv) (l1, ll2)
     else a2::union2 (order,equiv) (ll1, l2) ;;
				 
union2 ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) (['g'], ['a';'g';'z']) ;;
  
union2 ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) (['g';'h'], ['g';'h']) ;;

union2 ((fun (x,y) -> x < y),(fun (x,y) -> x = y)) ([1;2;3;4;5],[3;4;5;6]) ;;
  
(* 2.3.4 Searching in a List *)

type 'a option = None
	       | Some of 'a ;;  
  
let rec find prop list =
  match list with
    [] -> None
  | (a::l) -> if prop a then Some a else find prop l ;;

find ( fun x -> x = 1 ) [2;3;1] ;;

find ( fun x -> x = 100 ) [2;3;1] ;;

find (fun x -> (x mod 2 ) = 0 ) [3;1;7;8;13;4] ;;  

find (fun x -> (x mod 2 ) = 0 ) [3;1;7;13] ;;  

let associate v l =
  match find (fun (x,y) -> x = v) l with
    None -> None
  | Some (x,y) -> Some y ;;

associate 3 [(1,2);(3,4);(5,6)] ;;  

  
