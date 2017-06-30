
#mod_use "ch2.ml" ;;

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

print_inttree  (Node(Leaf 1,Node (Leaf 2,Node (Leaf 3, Leaf 4)))) ;;

(* new_printer has been replaced with install_printer
 * Thanks github.com/Fourchaux !
 *)
  
let rec print_inttree' = function
  | Leaf n -> Format.print_int n
  | Node (t1, t2) ->
    Format.print_string "("; print_inttree' t1; Format.print_string "," ;
    print_inttree' t2; Format.print_string ")" ;;

#install_printer print_inttree' ;;

Node (Leaf 3, Node (Leaf 4, Leaf 5)) ;;

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
  
match [< '1; '2; '3 >] with parser [< >] -> true;;
  
(* 4.4.1 Vectors *)

let v = [|1; 2; 3|] ;;

v ;;

Array.length v;;

(* BUG? replaced vect_length by Array.length but also reduced the
   upper bound by 1 *)  

let mem_vect cmp e v =
  let rec find m n =
    if m > n then false else
      let p = (m+n)/2 in
      if v.(p) = e then true else
	if cmp (v.(p)) e then find (p+1) n
	else find m (p-1)
  in
  find 0 ((Array.length v)-1) ;;

mem_vect ( < ) 7 [| 2; 3; 5; 6; 7; 10; 12; 15|] ;;

mem_vect ( < ) 16 [| 2; 3; 5; 6; 7; 10; 12; 15|] ;;

mem_vect ( < ) 1 [| 2; 3; 5; 6; 7; 10; 12; 15|] ;;
  
let v = [|0; 1; 2; 3|] ;;

v.(1) <- 7 ;;

v ;;
  
let swap v i j =
  let x = v.(i) in
  begin
    v.(i) <- v.(j) ;
    v.(j) <- x
  end ;;

swap v 1 2 ;;

v ;;

let place c v i j =
  let rec place_rec i' j' =
    let rec move_right p =
      if c (v.(i)) (v.(p)) || p = j' then p
      else move_right (p+1)
    and
      move_left p =
      if c (v.(p)) (v.(i)) || p = i' then p
      else move_left (p-1)
    in
    let k = move_right i' and l = move_left  j'
    in
    if k>l then (swap v i l; l)
    else
      if k=l then if c (v.(l)) (v.(i)) then (swap v i l; l)
		  else i 
      else (swap v k l; place_rec (k+1) (l-1))
  in
  place_rec (i+1) j ;;

let v = [|2; 5; 1; 3; 7; 5; 3; 5; 1; 9; 10; 2; 13|] ;;
(*        0  1  2  3  4  5  6  7  8  9  10  11 12     *)
place ( < ) v 0 ((Array.length v)-1);;
v ;;

let v = [|2; 5; 1; 3; 7; 5; 3; 5; 1; 9; 10; 2; 13|] ;;
place ( < ) v 1 ((Array.length v)-1);;
v ;;

let v = [|2; 5; 1; 3; 7; 5; 3; 5; 1; 9; 10; 2; 13|] ;;
place ( < ) v 3 ((Array.length v)-1);;
v ;;

let v = [|2; 5; 1; 3; 7; 5; 3; 5; 1; 9; 10; 2; 13|] ;;
place ( < ) v 9 ((Array.length v)-1);;
v ;;

  
let quicksort c v =
  let rec quick i j =
    if i<j then
      let p = place c v i j
      in
      quick i (p-1);
      quick (p+1) j
  in
  quick 0 ((Array.length v)-1) ;;

let v = [| 18; 2; 13; 4; 6; 25; 1; 10; 12; 9; 15; 7; 3|];;  

quicksort ( < ) v ;;
  
v ;;

quicksort ( > ) v ;;
  
v ;;
  
  
(* Records with Modifiable Fields *)

type planar_point = { mutable xcoord: float; mutable ycoord: float } ;;
  
let translate (dx, dy) pt =
  pt.xcoord <- pt.xcoord +. dx ;
  pt.ycoord <- pt.ycoord +. dy ;
  pt ;;
  
let point = {xcoord=2.0; ycoord=2.0} ;;

translate (1.2, 2.8) point ;;  

point ;;

let pt1 = {xcoord=2.0; ycoord=2.0} ;;

let pt2 = {xcoord=2.0; ycoord=2.0} ;;
  
let pt3 = pt1 ;;

pt1 = pt2 ;;  

pt1 = pt3 ;;  

pt1 == pt2 ;;

pt1 == pt3 ;;    

pt1.xcoord <- 1.0 ;;

pt1 ;;

pt3 ;;

pt2 ;;

(fun ((x,y),z) -> (x,y)) ((1,2),3) ;;
  
(fun ((x,y) as t,z) -> t) ((1,2),3) ;;

(* 4.4.3 References *)

let c = ref 0 ;;
  
let s = ref "Hello" ;;

let l = [ref 1; ref 2; ref 3] ;;

!c ;;

!(List.hd l) ;;

c:= !c + 3 ;;

c ;;

List.hd (List.tl l) := 7 ;;

l ;;

let gensym =
  let count = ref (-1) in
  fun() -> count := !count + 1;
	   "ident" ^ (string_of_int !count) ;;

gensym() ;;

gensym() ;;

gensym() ;;

  
(* 4.4.4 Circular lists *)

type 'a lnode = { info: 'a; mutable next: 'a lnode} ;;

let mk_circular_list e =
  let rec x = {info=e; next=x}
  in x ;;

let last ln = ln.info ;;

let first ln = (ln.next).info ;;

let test = mk_circular_list 1 ;;

first test ;;

last test ;;

let insert_head e l =
  let x = {info=e; next=l.next}
  in l.next <- x;
     l ;;
  
insert_head 2 test ;;

first test ;;

last test ;;  
  
let insert_tail e l =
  let x={info=e; next=l.next}
  in l.next <- x;
     x;;

let elim_head l =
  l.next <- (l.next).next;
  l ;;

let l = mk_circular_list 1
    in Ch2.list_it insert_tail [5;4;3;2] l;;

type 'a queue = Emptyqueue
	      | Queue of 'a lnode ;;

let enqueue x = function
    Emptyqueue -> Queue (mk_circular_list x)
  | (Queue ln) -> Queue (insert_tail x ln) ;;

let q = enqueue 3 (enqueue 2 (enqueue 1 Emptyqueue));;
  
let dequeue = function
    Emptyqueue -> failwith "dequeue: queue is empty"
  | (Queue ln) -> if ln.next == ln then (Emptyqueue, ln.info)
		  else let x = first ln
		       in
		       (Queue (elim_head ln), x) ;;

let list_of_queue = function
    Emptyqueue -> []
  | (Queue ln) -> let ln1 = ln.next in
		  let rec loq ln =
		    if ln == ln1 then []
		    else ln.info :: loq ln.next
		  in
		  ln1.info::loq ln1.next ;;

list_of_queue Emptyqueue ;;

list_of_queue q ;;

let (q1, _) = dequeue q
    in list_of_queue q1 ;;  
  
(* 4.4.5 Double Linked Lists *)
  
type 'a dblnode = {info: 'a;
		   mutable prev: 'a dblnode;
		   mutable next: 'a dblnode} ;;

let mk_dbl_circular_list e =
  let rec x = {info=e; prev=x; next=x}
  in x ;;

let dbl = mk_dbl_circular_list 1 ;;  
  
let insert_before e l =
  let lprev = l.prev in
  let x = {info=e; prev=lprev; next=l} in
  lprev.next <- x;
  l.prev <- x ;;

insert_before 2 dbl ;;  
insert_before 3 dbl ;;  
  
let insert_after e l =
  let lnext = l.next in
  let x = {info=e; prev=l; next=lnext} in
  lnext.prev <- x;
  l.next <- x ;;
  
insert_after 4 dbl ;;  
insert_after 5 dbl ;;  

let elim l =
  let lprev = l.prev
    and lnext = l.next
  in
  lprev.next <- lnext;
  lnext.prev <- lprev;
  lprev ;;

(* 4.5 Semantics of Destructive Operations *)

let sym c = ("ident" ^ (string_of_int c), c+1) ;;

let (mysym, c) = sym 0 in
    sym c ;;

let compose c f g x =
  let (v1, c1) = g c x
  in
  f c1 v1 ;;

let update mem loc val_ = 
  fun loc' -> if loc' = loc then val_ else mem loc ;;
