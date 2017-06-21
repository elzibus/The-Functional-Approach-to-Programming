
(* exercise 2.1 *)

(* TODO *)

(* exercise 2.2 *)

(* https://en.wikipedia.org/wiki/Complex_number#Elementary_operations *)
(* more in https://github.com/ocsigen/js_of_ocaml/blob/master/examples/hyperbolic/hypertree.ml *)
(* examples from here: http://www.mesacc.edu/~scotz47781/mat120/notes/ *)

type complex = { re_part: float; im_part: float } ;;

let cx_conjugate {re_part=re; im_part=im} = {re_part=re; im_part=(-1.*.im)} ;;
  
cx_conjugate {re_part=1.; im_part=2.} ;;
  
let cx_add {re_part=re1; im_part=im1} {re_part=re2; im_part=im2} =
  {re_part = re1 +. re2; im_part = im1 +. im2} ;;

cx_add {re_part=1.; im_part=2.} {re_part=3.; im_part=4.} ;;

let cx_sub {re_part=re1; im_part=im1} {re_part=re2; im_part=im2} =
  {re_part = re1 -. re2; im_part = im1 -. im2} ;;

cx_sub {re_part=1.; im_part=2.} {re_part=3.; im_part=4.} ;;

let cx_mul {re_part=re1; im_part=im1} {re_part=re2; im_part=im2} =
  {re_part= re1 *. re2 -. im1 *. im2; im_part = im1 *. re2 +. re1 *. im2} ;;

cx_mul {re_part = 4.; im_part = - 3.}
       {re_part = 2.; im_part = 5.} ;;

let cx_sq_norm {re_part=re1; im_part=im1} =
  re1 *. re1 +. im1 *. im1 ;;
  
let cx_div num denom =
  let n = cx_sq_norm denom in
  { re_part = (num.re_part *. denom.re_part +. num.im_part *. denom.im_part) /. n;
    im_part = (num.im_part *. denom.re_part -. num.re_part *. denom.im_part) /. n } ;;

cx_div {re_part=3. ; im_part= 2.}
       {re_part=4. ; im_part= - 3.} ;;
    
let cx_norm c = sqrt( cx_sq_norm c );;

(* exercise 2.3 *)

let rec it_list f e l =
  match l with
    [] -> e
  | (a::l) -> it_list f (f e a) l ;;
  
let maxi cmp l = it_list (fun a b -> if (cmp a b) then a else b) (List.hd l) (List.tl l) ;;
  
maxi ( > ) [1;2;3;4;5;6;7;8] ;;
  
maxi ( > ) [10;1;2;3;4;5;6;7;8] ;;

maxi ( > ) ['a';'m';'O'] ;;

(* exercise 2.4 *)
(* returns the pair (min,max) *)

(* wasted I don't know how long here because I had a syntax error due to a missing double semi-colon in the last expression above.... *)  
  
let minmax gt l = it_list (fun (a,b) c -> let p1 = if (gt a c) then c else a in
					  let p2 = if (gt c b) then c else b in
					  (p1,p2))
			  (List.hd l, List.hd l)
			  (List.tl l) ;;

minmax ( > ) [1;2;3;2;1;3;2;4;6;78] ;;

(* exercise 2.5 *)  

let rec list_it f l e =
  match l with
    [] -> e
  | (a::l) -> f a (list_it f l e) ;;

let partition test l =
  let switch elem (l1, l2) =
    if test elem then (l1, elem::l2) else (elem::l1, l2)
  in list_it switch l ([],[]) ;;

let rec quicksort order l =
  match l with
    [] -> []
  | a::l -> let (l1,l2) = partition (order a) l in
	    (quicksort order l1) @ (a::(quicksort order l2)) ;;
  
let pair1sort = quicksort ( fun (x,_) (y,_) -> x < y) ;;

pair1sort [(5,6);(1,2);(1,3);(2,4)] ;;

pair1sort [(1,2);(1,3);(1,1);(10,10);(2,4)] ;;

(* exercise 2.6 *)  
  
let lexisort cmp1 cmp2 =
  quicksort (fun (x1,y1) (x2,y2) -> (cmp1 x1 x2) || ( x1 = x2 && (cmp2 y1 y2))) ;;

lexisort ( < ) ( < ) [(1,2);(1,3);(1,1);(10,10);(2,4)] ;;
  
lexisort ( < ) ( < ) [(1,'2');(1,'3');(1,'1');(10,'a');(0,'0');(2,'4')] ;;

lexisort ( < ) ( > ) [(1,'2');(1,'3');(1,'1');(10,'a');(0,'0');(2,'4')] ;;
