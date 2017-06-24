
#mod_use "ch2.ml" ;;

open Ch2  
  
(* exercise 2.1 *)

(* TODO *)

(* exercise 2.2 *)

(* https://en.wikipedia.org/wiki/Complex_number#Elementary_operations *)
(* more in https://github.com/ocsigen/js_of_ocaml/blob/master/examples/hyperbolic/hypertree.ml *)
(* examples from here: http://www.mesacc.edu/~scotz47781/mat120/notes/ *)

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
  
let maxi cmp l = it_list (fun a b -> if (cmp a b) then a else b) (List.hd l) (List.tl l) ;;
  
maxi ( > ) [1;2;3;4;5;6;7;8] ;;
  
maxi ( > ) [10;1;2;3;4;5;6;7;8] ;;

maxi ( > ) ['a';'m';'O'] ;;

(* exercise 2.4 *)
(* returns the pair (min,max) *)

let minmax gt l = it_list (fun (a,b) c -> let p1 = if (gt a c) then c else a in
					  let p2 = if (gt c b) then c else b in
					  (p1,p2))
			  (List.hd l, List.hd l)
			  (List.tl l) ;;

minmax ( > ) [1;2;3;2;1;3;2;4;6;78] ;;

(* exercise 2.5 *)  
  
let pair1sort = quicksort ( fun (x,_) (y,_) -> x < y) ;;

pair1sort [(5,6);(1,2);(1,3);(2,4)] ;;

pair1sort [(1,2);(1,3);(1,1);(10,10);(2,4)] ;;

(* exercise 2.6 *)  
  
let lexisort cmp1 cmp2 =
  quicksort (fun (x1,y1) (x2,y2) -> (cmp1 x1 x2) || ( x1 = x2 && (cmp2 y1 y2))) ;;

lexisort ( < ) ( < ) [(1,2);(1,3);(1,1);(10,10);(2,4)] ;;
  
lexisort ( < ) ( < ) [(1,'2');(1,'3');(1,'1');(10,'a');(0,'0');(2,'4')] ;;

lexisort ( < ) ( > ) [(1,'2');(1,'3');(1,'1');(10,'a');(0,'0');(2,'4')] ;;

(* exercise 2.7 *)

(* a0 * x^0 + a1 * x^1 + a2 * x^2 + ... *)
(* [] is equivalent to [0] *)
  
let string_of_monom coef deg =
  if deg = 0 then string_of_float coef
  else if deg = 1 then string_of_float coef ^ "*" ^ "x"
  else if coef = 1.0 then "x^" ^ (string_of_int deg)
  else (string_of_float coef) ^ "*" ^ "x^" ^ (string_of_int deg) ;;
  
let string_of_polynom polynom =
  if polynom = [] || polynom = [0.0] then "0.0"
  else let rec sop_helper p deg res =
	 match p with
	   [] -> res
	 | (a::pr) -> if a = 0.0 then sop_helper pr (deg+1) res
		      else if res = "" then sop_helper pr (deg+1) (string_of_monom a deg)
		      else sop_helper pr (deg+1) (res ^ " + " ^ string_of_monom a deg)
       in
       sop_helper polynom 0 "" ;;
  
string_of_polynom [1.0;2.0;1.0] ;;
    
string_of_polynom [0.0;0.0;4.0;0.0;0.0;0.0;0.0;8.0] ;;
  
let rec add_polynom p1 p2 =
  match (p1,p2) with
    ([],p2) -> p2
  | (p1,[]) -> p1
  | (a::pr1,b::pr2) -> (a+.b)::add_polynom pr1 pr2 ;;

let p = add_polynom [1.0;0.0;4.0;0.0;0.0;0.0;0.0;0.0;5.0] [1.0;2.0;1.0] in
    string_of_polynom p ;;
  
(* prefixing a polynom in this representation with a 0.0 is the same as multiplying by x *)
  
let rec mul_polynom p1 p2 =
  match (p1,p2) with
    ([],p2) -> [0.0]
   |(p1,[]) -> [0.0]
   |((p1a::p1l),p2) -> add_polynom (map (fun elt -> p1a *. elt ) p2)
				   (mul_polynom p1l (0.0::p2)) ;;

let p = mul_polynom [1.0] [1.0] in
    string_of_polynom p ;;
  
let p = mul_polynom [1.0;0.0;4.0;0.0;0.0;0.0;0.0;0.0;5.0] [1.0;2.0;1.0] in
    string_of_polynom p ;;

let p = mul_polynom [1.0;2.0;1.0] [1.0;0.0;4.0;0.0;0.0;0.0;0.0;0.0;5.0] in
    string_of_polynom p ;;

let p =  mul_polynom [-1.0] [1.0;0.0;4.0;0.0;0.0;0.0;0.0;0.0;5.0] in
    string_of_polynom p ;;

let p = mul_polynom [2.0;3.0] [5.0;- 7.0;4.0] in
    string_of_polynom p ;;
	
let eval_polynom p x =
  let rec ep_helper p deg =
    match p with
      [] -> 0.0
    |(a::l) -> a *. (x ** deg) +. ep_helper l (deg +. 1.0)
  in
  ep_helper p 0.0 ;;
  
eval_polynom [0.0;1.0;1.0] 1.0 ;;

eval_polynom [10.0] 100.0 ;;

eval_polynom [6.0;0.0;- 1.0; 3.0; 1.0] (- 3.0) ;;
  
let deriv_polynom p =
  let lp = List.length p in
  if lp = 0 || lp = 1 then [0.0]
  else let rec dp_helper p i =
	 match p with
	   [] -> []
	 | (a::l) -> (a *. i)::dp_helper l (i+.1.0)
       in
       dp_helper (List.tl p) 1.0 ;;

let p =  deriv_polynom [1.0;0.0;4.0;0.0;0.0;0.0;0.0;0.0;5.0] in
    string_of_polynom p ;;

let p =  deriv_polynom [0.0] in
    string_of_polynom p ;;

let p =  deriv_polynom [0.0;0.0;1.0] in
    string_of_polynom p ;;

(* (+ constant) won't be handled.. *)  
let integr_polynom p =
  let lp = List.length p in
  if lp = 0 || p = [0.0] then [0.0]
  else let rec ip_helper p i =
	 match p with
	   [] -> []
	 | (a::l) -> let elt = if i = 0.0 then a else (a /. i) in
		     elt::ip_helper l (i+.1.0)
       in
       ip_helper (0.0::p) 0.0 ;;

let p = integr_polynom [1.0;0.0;2.0;1.0;0.0;4.0] in
    string_of_polynom p ;;

let p = integr_polynom [0.0] in
    string_of_polynom p ;;

let p = integr_polynom [5.0] in
    string_of_polynom p ;;

let p = integr_polynom [0.0;5.0] in
    string_of_polynom p ;;
  
  
(* exercise 2.8 *)

(* TODO *)  
  
(* exercise 2.9 *)


let set_of_tree equiv t =
  let rec sot_helper t set =
    match t with
      Leaf l -> add_to_set equiv l set
    | Node (n1, n2) -> union equiv (sot_helper n1 set, sot_helper n2 set)
  in
  sot_helper t [] ;;

set_of_tree ( = ) (Leaf 12) ;;  

set_of_tree ( = ) (Node (Leaf 12, Node (Leaf 28, Node (Leaf 34, Leaf 12)))) ;;

let t1 = inttree_of_string "(1,(2,3))" in
    set_of_tree ( = ) t1;;  

let t1 = inttree_of_string "(1,(2,(3,(4,(4,(3,(2,1)))))))" in
    set_of_tree ( = ) t1;;  
  
(* exercise 2.10 *)

type direction = L | R ;;

let subtree_from_ldir ldir t =
  let rec sfl_helper ldir t step1 =
    match (ldir,t) with
      ([],t) -> Some t
     |([L], Leaf l) -> if not step1 then Some (Leaf l) else None
     |([R], Leaf l) -> if not step1 then Some (Leaf l) else None
     |([L], Node(n1,_)) -> Some n1
     |([R], Node(_,n2)) -> Some n2
     |(L::rd, Node(n1,n2)) -> sfl_helper rd n1 false
     |(R::rd, Node(n1,n2)) -> sfl_helper rd n2 false
  in
  sfl_helper ldir t true ;;
    
subtree_from_ldir [R;L] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

subtree_from_ldir [R;R] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

subtree_from_ldir [R;R;R] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

subtree_from_ldir [R] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;
  
subtree_from_ldir [] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

(* My understanding from the u=u1u2 rule in the exercise is that
 * the following call needs to return false. By this I mean u1 cannot be empty:
 * at least one step needs to be taken in the tree.
 * step1 is a bool that encodes this information in the function definition.
 *)
  
subtree_from_ldir [R] (Leaf 5) ;;

subtree_from_ldir [L] (Leaf 5) ;;

subtree_from_ldir [] (Leaf 5) ;;

subtree_from_ldir [R;R]
		  (Node(Leaf 1,Node(Leaf 2, Leaf 3))) ;;

subtree_from_ldir [R;R;R;R;R;R;R;R]
		  (Node(Leaf 1,Node(Leaf 2,Node(Leaf 3,Node(Leaf 4,Node(Leaf 5,Node(Leaf 6,Node(Leaf 7,Node(Leaf 8, Leaf 9)))))))));;

subtree_from_ldir [R;R;R;R;R;R;R;L]
		  (Node(Leaf 1,Node(Leaf 2,Node(Leaf 3,Node(Leaf 4,Node(Leaf 5,Node(Leaf 6,Node(Leaf 7,Node(Leaf 8, Leaf 9)))))))));;

(* exercise 2.11 *)

let walk_tree_ldir dl root =
  let rec wtl_helper dl' t res pass1 =
    match (dl',t) with
      ([], Leaf l) -> if not pass1 then Some (res @ [l]) else None
     |(ds, Leaf l) -> wtl_helper ds root (res @ [l]) false
     |([], Node (_,_)) -> None
     |(d::ds), Node(n1, n2) -> if d = L then wtl_helper ds n1 res false else wtl_helper ds n2 res false
  in
  wtl_helper dl root [] true ;;

let t = Node(Node(Node(Leaf "A", Leaf "B"), Node(Leaf "C", Leaf "D")), Leaf "E") in
    walk_tree_ldir [L;L;R;R;L;R;L] t ;;

let t = Node(Node(Node(Leaf "A", Leaf "B"), Node(Leaf "C", Leaf "D")), Leaf "E") in
    walk_tree_ldir [L;R;R;  R;  L;R;L; L;L;L; L;R;R; R] t ;;

let t = Node(Node(Node(Leaf "A", Leaf "B"), Node(Leaf "C", Leaf "D")), Leaf "E") in
    walk_tree_ldir [R;R;L] t ;;
  
(* exercise 2.12 *)
  

