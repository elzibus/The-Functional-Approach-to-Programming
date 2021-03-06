
#mod_use "ch2.ml" ;;

open Ch2  
  
(* exercise 2.1 *)

let pp_translate point x y =
  {xcoord = point.xcoord +. x;
   ycoord = point.ycoord +. y} ;;
  
pp_translate {xcoord=0.0; ycoord=0.0} 1.0 2.0 ;;
  
let pi = 4.0 *. atan 1.0;;

(* theta in radians, rotation counter-clockwise around the origin *)
let pp_rotate point theta =
  let x = point.xcoord in
  let y = point.ycoord in
  {xcoord = x *. (cos theta) -. y *. (sin theta);
   ycoord = x *. (sin theta) +. y *. (cos theta)} ;;
  
pp_rotate {xcoord=2.3;ycoord=5.6} 0.0 ;;

pp_rotate {xcoord=0.0;ycoord=0.0} 110.0 ;;

pp_rotate {xcoord=1.0;ycoord=0.0} (pi/.2.0) ;;
  
pp_rotate {xcoord=1.0;ycoord=0.0} pi ;;

pp_rotate {xcoord=1.0;ycoord=0.0} (2.0*.pi) ;;

let pp_scale point factor =
  {xcoord = point.xcoord *. factor;
   ycoord = point.ycoord *. factor} ;;

pp_scale {xcoord=0.0; ycoord=0.0} 10.0 ;;  

pp_scale {xcoord=10.0; ycoord=10.0} 0.0 ;;  

pp_scale {xcoord=1.0; ycoord=2.0} 10.0 ;;  
  
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

(* a2 * x^2 + ... *)
(* [(a2, 2); .. ] *)

let string_of_monom2 m =
  let coef = fst m in 
  let deg = snd m in
  if deg = 0 then string_of_float coef
  else if deg = 1 then string_of_float coef ^ "*" ^ "x"
  else if coef = 1.0 then "x^" ^ (string_of_int deg)
  else (string_of_float coef) ^ "*" ^ "x^" ^ (string_of_int deg) ;;
  
let string_of_polynom2 polynom =
  let rec sop2_helper p res =
    match p with
      [] -> res
    | (a::pr) -> if res = "" then sop2_helper pr (string_of_monom2 a)
		 else sop2_helper pr (res ^ " + " ^ string_of_monom2 a)
  in
  sop2_helper polynom "" ;;
  
string_of_polynom2 [(1.0, 0);(2.0,1);(1.0,2)] ;;

let rec add_polynom2 p1 p2 =
  match (p1,p2) with
    ([],p2) -> p2
  | (p1,[]) -> p1
  | ((c1,d1)::pr1,(c2,d2)::pr2) -> if d1 = d2 then (c1 +. c2,d1)::add_polynom2 pr1 pr2
				   else if d1 < d2 then (c1,d1)::add_polynom2 pr1 p2
				   else (c2,d2)::add_polynom2 p1 pr2 ;;

let p = add_polynom2 [(1.0,0);(4.0,2);(5.0,8)] [(1.0,0);(2.0,1);(1.0,2)]
    in string_of_polynom2 p ;;

let p = add_polynom2 [(1.0,0)] [(1.0,50)]
    in string_of_polynom2 p ;;

let rec mul_polynom2 p1 p2 =
  match (p1,p2) with
    ([],p2) -> []
   |(p1,[]) -> []
   | ((c1,d1)::pr1, p2) -> add_polynom2 (mul_polynom2 pr1 p2)
					(map (fun (x,y) -> (x*.c1, d1+y)) p2) ;;

let p = mul_polynom2 [(1.0,0)] [(1.0,0)] in
    string_of_polynom2 p ;;
  
let p = mul_polynom2 [(1.0,0);(4.0,2);(5.0,8)] [(1.0,0);(2.0,1);(1.0,2)]
    in string_of_polynom2 p ;;

let p = mul_polynom2 [(1.0,0);(4.0,2);(5.0,8)] [(1.0,0);(2.0,1);(1.0,2)]
    in string_of_polynom2 p ;;

let p =  mul_polynom2 [(-1.0,0)] [(1.0,0);(4.0,2);(5.0,8)] in
    string_of_polynom2 p ;;

let p = mul_polynom2 [(2.0,0);(3.0,1)] [(5.0,0);(- 7.0,1);(4.0,2)] in
    string_of_polynom2 p ;;

let eval_polynom2 p x =
  let rec ep2_helper p =
    match p with
      [] -> 0.0
    |((c,d)::l) -> c *. (x ** (float_of_int d)) +. ep2_helper l
  in
  ep2_helper p ;;
  
eval_polynom2 [(0.0,0);(1.0,1);(1.0,2)] 1.0 ;;

eval_polynom2 [(10.0,0)] 100.0 ;;

eval_polynom2 [(6.0,0);(0.0,1);(- 1.0,2); (3.0,3); (1.0,4)] (- 3.0) ;;

let rec deriv_polynom2 p =
  match p with
    [] -> []
  | ((c,d)::l) -> if d = 0 then deriv_polynom2 l
		  else (c *. (float_of_int d), d-1)::deriv_polynom2 l ;;
  
let p =  deriv_polynom2 [(1.0,0);(4.0,2);(5.0,8)]
    in string_of_polynom2 p ;;

let p =  deriv_polynom2 [(1.0,2)] in
    string_of_polynom2 p ;;

let p =  deriv_polynom2 [(1.0, 0)] in
    string_of_polynom2 p ;;

let rec integr_polynom2 p =
  match p with
    [] -> []
  | ((c,d)::l) -> let c = if d = 0 then c else (c /. ((float_of_int d) +. 1.0)) in
		  (c, d + 1)::integr_polynom2 l ;;
  
let p = integr_polynom2 [(1.0,0);(2.0,2);(1.0,3);(4.0,5)] in
    string_of_polynom2 p ;;
  
let p = integr_polynom2 [(1.0,0)] in
    string_of_polynom2 p ;;

let p = integr_polynom2 [(5.0,1)] in
    string_of_polynom2 p ;;
  
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
      ([],t) -> if not step1 then Some t else None
     |(_, Leaf l) -> if not step1 then Some (Leaf l) else None
     |(L::rd, Node(n1,n2)) -> if rd = [] then Some n1 else sfl_helper rd n1 false
     |(R::rd, Node(n1,n2)) -> if rd = [] then Some n2 else sfl_helper rd n2 false
  in
  sfl_helper ldir t true ;;
    
subtree_from_ldir [R;L] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

subtree_from_ldir [R;R] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

subtree_from_ldir [R;R;R] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

subtree_from_ldir [R] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;
  
(* My understanding from the u=u1u2 rule in the exercise is that
 * the following call needs to return false. By this I mean u1 cannot be empty:
 * at least one step needs to be taken in the tree.
 * step1 is a bool that encodes this information in the function definition.
 *)

subtree_from_ldir [] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;
  
subtree_from_ldir [R] (Leaf 5) ;;

subtree_from_ldir [R;R]
		  (Node(Leaf 1,Node(Leaf 2, Leaf 3))) ;;

subtree_from_ldir [R;R;R;R;R;R;R;R]
		  (Node(Leaf 1,Node(Leaf 2,Node(Leaf 3,Node(Leaf 4,Node(Leaf 5,Node(Leaf 6,Node(Leaf 7,Node(Leaf 8, Leaf 9)))))))));;

subtree_from_ldir [R;R;R;R;R;R;R;L]
		  (Node(Leaf 1,Node(Leaf 2,Node(Leaf 3,Node(Leaf 4,Node(Leaf 5,Node(Leaf 6,Node(Leaf 7,Node(Leaf 8, Leaf 9)))))))));;

subtree_from_ldir [L;L;L;L;L] (Node(Leaf 3,Node(Leaf 4, Leaf 5))) ;;

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
  
let lorem_ipsum = "lorem ipsum dolor sit amet, consectetur adipiscing elit. mauris eget elit semper, molestie ligula a, sagittis sem. quisque ullamcorper neque libero, in elementum arcu malesuada non. ut in eleifend sapien. etiam a cursus velit. donec nec lorem ac metus commodo aliquam. mauris semper nisi et ante placerat scelerisque. vestibulum fringilla efficitur sollicitudin. suspendisse quis ante vehicula, tincidunt ligula ut, viverra justo. etiam auctor rutrum arcu nec eleifend. etiam venenatis vitae ipsum at pretium. etiam non vestibulum orci. vivamus ultrices tortor sit amet ante porttitor, id volutpat sapien dapibus. etiam dapibus varius metus sit amet venenatis. etiam consequat justo a convallis ullamcorper." ;;

String.length lorem_ipsum ;;

(* break it into a list of characters *)

let string_to_lchar str =
  let rec stl_helper str i res =
    if i = String.length str then res
    else stl_helper str (i+1) (res @ [str.[i]])
  in
  stl_helper str 0 [] ;;
  
let char_counts lchar =
  let rec cc_helper lchar cur count =
    match lchar with
      [] -> [(cur, count)]
     |(c::cs)-> if c = cur then cc_helper cs cur (count+1)
		else (cur, count)::cc_helper (c::cs) c 0
  in
  cc_helper lchar (List.hd lchar) 0 ;;

(* ! if I use directly "<" below for the comparison, I get a '_a which
   specializes to char after its first use...
   passing an "order" function solves this *)
  
let pair_sort_snd order =  quicksort ( fun (_,x) (_,y) -> order x y ) ;;
  
let rec build_code pairs =
  match pairs with
    [] -> failwith "no input"
   |[p] -> fst p
   |(p1::p2::ps) -> let new_pair = (Node( fst(p1), fst(p2)), snd(p1) +. snd(p2)) in
		    build_code (pair_sort_snd ( < ) ([new_pair] @ ps)) ;;

let sorted_list_char = quicksort ( < ) (string_to_lchar lorem_ipsum) ;;

let counts = char_counts sorted_list_char ;;

let total = sigma (map (fun (_,y) -> y) counts) ;;

let pair_prob = map (fun (x,y) -> (Leaf x, float_of_int y /. float_of_int total)) counts ;;

let rec sumf l =
  match l with
    [] -> 0.0
   |(x::xs) -> x +. sumf xs ;;

(* sum should be 1.0 *)  
sumf (map (fun elt -> snd(elt)) pair_prob) ;;

(* now the pairs need to be sorted according to the second element *)
  
let pair_prob_sorted = pair_sort_snd ( < ) pair_prob ;;

build_code pair_prob_sorted ;;

build_code [(Leaf 12, 0.5); (Leaf 54, 0.5)] ;;

let get_symbol_codes t =
  let rec gs_helper t path res =
    match t with
    | Leaf l -> (Leaf l, path)::res
    | Node ( n1, n2) -> (gs_helper n1 (path @ [L]) res)
			@ (gs_helper n2 (path @ [R]) res)
  in
  gs_helper t [] [] ;;  

get_symbol_codes (build_code pair_prob_sorted) ;;

walk_tree_ldir [L; L; R; L;                      (* "l" *)
		R; R; L; L; R;                   (* "o" *)
		L; L; L; R;                      (* "r" *)
		L; R; L;                         (* "e" *)
		L;L;R;R]                         (* "m" *)
	       (build_code pair_prob_sorted) ;;

get_symbol_codes (build_code [(Leaf 12, 0.5); (Leaf 54, 0.5)]) ;;
  
let get_binary_codes t =
  let rec gs_helper t path res =
    match t with
    | Leaf l -> (Leaf l, path)::res
    | Node ( n1, n2) -> (gs_helper n1 ("0" ^ path) res)
			@ (gs_helper n2 ("1" ^ path) res)
  in
  gs_helper t "" [] ;;  

(* in the following code, space is the most frequent char in
 * the input string and gets the shortest code, as expected *)
get_binary_codes (build_code pair_prob_sorted) ;;

get_binary_codes (build_code [(Leaf 12, 0.5); (Leaf 54, 0.5)]) ;;
  
