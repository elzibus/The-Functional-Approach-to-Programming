(* This chapter 9 code is to be compiled and uses nuklear for the graphics *)

(* ----------------------------- *)

(* 9.1.1 Geometric Plane *)

type point = { xc: float; yc: float } ;;

let origin = {xc= 0.0; yc= 0.0} ;;

(* ----------------------------------------------------------------------------------- *)
(* FFI *)

type context
type canvas

type color = { r: int ; g: int; b: int} ;;
type rect = { x: float; y: float; w: float; h: float } ;;
 
external layout_row_static: context -> float -> int -> int -> unit = "ocaml_nk_layout_row_static"
external layout_row_dynamic: context -> float -> int -> unit = "ocaml_nk_layout_row_dynamic"
external button_label: context -> string -> int = "ocaml_nk_button_label"
external label: context -> string -> int -> unit = "ocaml_nk_label"
external slider_pct: context -> string -> int ref -> int -> float -> unit = "ocaml_nk_property_int"
external gui_begin: context -> string -> float -> float -> float -> float -> int -> int = "dummy" "ocaml_nk_begin"
external gui_end: context -> unit = "ocaml_nk_end"
external get_canvas: context -> canvas = "ocaml_nk_window_get_canvas"
external get_region: context -> rect = "ocaml_nk_get_region"
external line: canvas -> float -> color -> point -> point -> unit = "ocaml_nk_stroke_line"
external curve: canvas -> float -> color -> point -> point -> point -> point -> unit = "dummy" "ocaml_nk_stroke_curve"

(* 9.1.2 Geometric elements *)

type geom_element =
    Seg of point list
  | Arc of point * float * float * float
  | Curve of point * point * point * point ;;

type transformation = { m11: float; m12: float; m13: float;
		        m21: float; m22: float; m23: float } ;;

let transform_point =
  fun {m11=a; m12=b; m13=c; m21=d; m22=e; m23=f}
      {xc=x; yc=y} ->
  {xc=a*.x +. b *. y +. c; yc = d *. x +. e *. y +. f} ;;

let id_trans = {m11=1.0; m12=0.0; m13=0.0; m21=0.0; m22=1.0; m23=0.0} ;;

transform_point id_trans {xc = 123.0 ; yc = 456.0 } ;;

(* taken from Chapter 2 *)

let rec it_list f e l =
  match l with
    [] -> e
  | (a::l) -> it_list f (f e a) l ;;

let compose_transformations =
  let compose = fun
  {m11=a11; m12=a12; m13=a13; m21=a21; m22=a22; m23=a23}
  {m11=b11; m12=b12; m13=b13; m21=b21; m22=b22; m23=b23}
  -> {m11 = a11 *. b11 +. a12 *. b21 ;
      m12 = a11 *. b12 +. a12 *. b22 ;
      m13 = a11 *. b13 +. a12 *. b23 +. a13 ;
      m21 = a21 *. b11 +. a22 *. b21 ;
      m22 = a21 *. b12 +. a22 *. b22 ;
      m23 = a21 *. b13 +. a22 *. b23 +. a23 }
  in
  it_list compose id_trans ;;

transform_point (compose_transformations [id_trans; id_trans; id_trans])
		{xc=123.0 ; yc=456.0} ;;

let translation tx ty = {m11=1.0; m12=0.0; m13=tx;
			 m21=0.0; m22=1.0; m23=ty} ;;

transform_point (translation 1.0 2.0)
		{xc=123.0 ; yc=456.0} ;;

let pi = 4.0 *. atan 1.0;;

let deg_to_rad deg = deg *. pi /. 180.0 ;;

let sin_deg deg = sin( deg_to_rad deg ) ;;
let cos_deg deg = cos( deg_to_rad deg ) ;;

(* that's a rotation around the origin (0.0) *)

let rotation_origin deg =
  let co = cos_deg(deg) in
  let si = sin_deg(deg) in
  {m11=co; m12= -. si; m13=0.0;
   m21=si; m22=    co; m23=0.0} ;;
  
transform_point (rotation_origin 90.0)
		{xc=123.0 ; yc=0.0} ;;

(* composed from right to left !!! *)
let rotation pt deg =
  compose_transformations [ translation pt.xc pt.yc ;
			    rotation_origin deg;
			    translation (-. pt.xc) (-. pt.yc)] ;;
						    
transform_point (rotation {xc = -. 2.0; yc = -. 2.0} 60.0)
		{xc=2.5 ; yc=0.0} ;;

transform_point (rotation {xc = 2.0; yc = -. 1.0} 90.0)
		{xc = 5.0 ; yc= -. 1.0} ;;

let xaxis_symmetry = {m11= 1.0 ; m12=    0.0 ; m13=0.0;
		      m21= 0.0 ; m22= -. 1.0 ; m23=0.0} ;;

transform_point xaxis_symmetry
		{xc=123.0 ; yc=110.0} ;;

let yaxis_symmetry = {m11= -. 1.0 ; m12= 0.0 ; m13=0.0;
		      m21=    0.0 ; m22= 1.0 ; m23=0.0} ;;

transform_point yaxis_symmetry
		{xc=123.0 ; yc=110.0} ;;

let origin_symmetry = compose_transformations [ xaxis_symmetry;
						yaxis_symmetry] ;;

transform_point origin_symmetry
		{xc=123.0 ; yc= -. 110.0} ;;

let point_symmetry pt =
  compose_transformations [ translation pt.xc pt.yc ;
			    origin_symmetry;
			    translation (-. pt.xc) (-. pt.yc)] ;;

transform_point (point_symmetry {xc= 6.0; yc= 4.0})
		{xc=4.0 ; yc= 5.0} ;;

let rad_to_deg rad = (180.0 *. rad) /. pi ;;

let angle_deg (pt1, pt2) =
  rad_to_deg (atan ((pt2.yc -. pt1.yc) /. (pt2.xc -. pt1.xc))) ;;

let p1 = {xc= 0.0; yc= 0.0} in
    let p2 = {xc= -5.0; yc= -6.0} in
    angle_deg (p1, p2) ;;

let line_symmetry (pt1, pt2) =
  let angle_deg = rad_to_deg ( atan ((pt2.yc -. pt1.yc) /. (pt2.xc -. pt1.xc)) ) in
  compose_transformations [ translation pt1.xc pt1.yc ;
			    rotation_origin angle_deg ;
			    xaxis_symmetry ;
			    rotation_origin (-. angle_deg) ;
			    translation (-. pt1.xc) (-. pt1.yc)] ;;
    
let p1 = {xc= -. 1.0; yc= -. 3.0} in
    let p2 = {xc= 5.0; yc= 2.0} in
    let p0 = transform_point (line_symmetry (p1, p2))
			     {xc=4.0 ; yc= 5.0} in
    transform_point (line_symmetry (p1, p2))
		    p0 ;;

let scaling (sx, sy) =
  {m11=sx ; m12=0.0; m13=0.0;
   m21=0.0; m22= sy; m23=0.0} ;;

transform_point (scaling (2.0, 3.0))
		{xc= 1.0 ; yc= -. 3.0} ;;

(* 9.1.3 Constructing Images *)

type sketch_part = Lift_pen
		 | Ge of geom_element list ;;

type sketch = sketch_part list ;;

(* I am converting Arc to segments
   Arc as it is defined with independent scaling in both x and y does not make sense
 *)

(* [min; min+step; ... ;max] *)
let range_values_step min max step =
  let rec rv_helper cur res =
    if cur>max then (List.rev res)
    else rv_helper (cur+.step) (cur::res)
  in
  rv_helper min [];;

(* [min; min+(max-min)/n; .. ; max *)
let range_values_n min max n =
  let step = (max -. min) /. (float_of_int n) in
  let rec rv_helper i res =
    if i = n then (List.rev (max::res))
    else rv_helper (i+1) ((min +. (float_of_int i) *. step)::res)
  in
  rv_helper 0 [];;

(* can be changed in the UI *)
let arc_to_segs_n = ref 20 ;;

let arc_to_segs pt r a1 a2 =
  let pts = List.map (fun a -> {xc=pt.xc +. r *. cos_deg(a);
				yc=pt.yc +. r *. sin_deg(a)})
		     (range_values_n a1 a2 !arc_to_segs_n) in
  Seg pts;;

let make_sketch gl =
  let rec ms_helper gl =
    match gl with
      [] -> []
    | (Seg pl)::rest -> (Seg pl)::ms_helper rest
    | Arc (pt,r,a1,a2)::rest -> (arc_to_segs pt r a1 a2) :: ms_helper rest
    | Curve (p1,p2,p3,p4) as curve :: rest -> curve :: ms_helper rest
  in
  Lift_pen :: [ Ge ( ms_helper gl) ] ;;

let group_sketches sl =
  List.flatten sl ;;

let rec transform_sketch tr sk =
  match sk with
    [] -> []
  | Lift_pen::rest -> Lift_pen :: transform_sketch tr rest
  | Ge g::rest -> (Ge (List.map ( fun elt -> match elt with
					   Seg pl -> Seg (List.map (fun pt -> transform_point tr pt) pl)
					 | Arc (pt,r,a1,a2) -> failwith "I don't expect an Arc here.."
					 | Curve (p1,p2,p3,p4) -> let p1' = transform_point tr p1 in
								  let p2' = transform_point tr p2 in
								  let p3' = transform_point tr p3 in
								  let p4' = transform_point tr p4 in
								  Curve (p1', p2', p3', p4'))
			    g)) :: transform_sketch tr rest;;

(* 9.2 Drawing Trees *)

(* 9.2.1 Drawing Principles *)

type picture_part = { linewidth : float;
		      color : color ;
		      sketch : sketch } ;;

type picture = picture_part list ;;

let make_blank_picture x y =
  [{linewidth=0.0; color={r=0;g=0;b=0}; sketch = []}] ;;

let make_picture (linew, col) sketch =
  [ { linewidth= linew ;
      color = col ;
      sketch = sketch } ] ;;

let group_pictures picl =
  (List.flatten  picl) ;;

let transform_picture tr pic =
  List.map (fun {linewidth; color; sketch} -> {linewidth=linewidth; color=color; sketch = transform_sketch tr sketch})
	   pic ;;

(*
 * the center of a Seg is the barycenter of all points that compose the Seg
 * the center of an Arc is its center
 * the center of a Curve is the barycenter of its constituting points
 *)

 let plist_center pl =
  let rec sc_helper l x y n =
    match l with
      [] -> if n > 0.0 then {xc = x /. n; yc = y/. n} else origin
    | (seg::xs) -> sc_helper xs (x +. seg.xc) (y +. seg.yc) (n +. 1.0)
  in
  sc_helper pl 0.0 0.0 0.0 ;;

plist_center [origin; {xc=1.0;yc=1.0}] ;;

(* center of a four point tuple *)

let fpoint_center (pt1, pt2, pt3, pt4) =
  {xc=(pt1.xc +. pt2.xc +. pt3.xc +. pt4.xc) /. 4.0;
   yc=(pt1.yc +. pt2.yc +. pt3.yc +. pt4.yc) /. 4.0} ;;

fpoint_center ({xc= -. 1.0; yc= -. 1.0},
	       {xc= -. 1.0; yc=    1.0},
	       {xc=    1.0; yc=    1.0},
	       {xc=    1.0; yc= -. 1.0}) ;;
let sk1 = 
  let ptA = {xc= -. 3.0 ; yc= -. 3.0 } in
  let ptB = {xc= -. 3.0 ; yc= -. 1.0 } in
  let ptC = {xc= -. 1.0 ; yc= -. 1.0 } in
  let ptD = {xc= -. 1.0 ; yc= -. 3.0 } in
  let ptE = {xc= -. 3.0 ; yc=    4.0 } in     (* BUG in the book : y(E) = 4.0 not -. 4.0 *)
  let ptF = {xc= -. 2.0 ; yc=    0.0 } in
  let ptG = {xc=    0.0 ; yc=    5.0 } in
  let ptH = {xc=    1.0 ; yc=    4.0 } in
  let ptI = {xc=    3.0 ; yc=    0.0 } in
  group_sketches [ (make_sketch [ Seg [ ptA; ptB; ptC; ptD; ptA] ]); 
		   (make_sketch [ Curve ( ptE, ptF, ptG, ptH) ]); 
		   (make_sketch [ Arc ( ptI, 2.0, 30.0, 290.0 ) ] ) ] ;;

(* computer the point at the center of a sketch - re-used by center_sketch and center_picture *)
let center_sketch_pt sk =
  let temp = List.map (fun skp -> match skp with
				    Lift_pen -> []
				  | Ge ge -> List.map (fun elt -> match elt with
								    Seg sl -> plist_center sl
								  | Arc (p,_,_,_) -> p
								  | Curve (p1,p2,p3,p4) -> fpoint_center (p1,p2,p3,p4))
						      ge)
		      sk
  in
  plist_center ( List.flatten temp) ;;

let center_sketch sk pt =
  let center = center_sketch_pt sk in
  transform_sketch (translation ( pt.xc -. center.xc) (pt.yc -. center.yc)) sk ;;

let center_picture pic pt =
  let non_empty_sketch = List.filter (fun {linewidth; color; sketch} -> sketch != []) pic in
  let lcenters = List.map (fun {linewidth; color; sketch} -> center_sketch_pt sketch)
			  non_empty_sketch in
  let center = plist_center lcenters in
  transform_picture (translation ( pt.xc -. center.xc) (pt.yc -. center.yc))
		    pic;;

type 'a btree = Empty
	      | Bin of 'a btree * 'a * 'a btree ;;

(* adapted from code in chapter 2 - still fragile .. *)

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

let is_var_char c = not ( c = ',' || c = '(' || c = ')') ;;

let read_var conv strp =
  let rec rv_helper strp res =
    if strp.ptr > (String.length strp.str) - 1 then (strp, conv res)
    else let c = peek_char strp in
	 if is_var_char c then rv_helper {str=strp.str; ptr=strp.ptr+1} (res ^ (String.make 1 c))
	 else  (strp, conv res)
  in
  rv_helper strp "" ;;

let btree_of_string conv str =
  let rec bos_helper strp =
    let strp1 = skip_spaces strp in
    if peek_char (strp1) = '(' then ( let strp2 = next_char strp1 in
				      if peek_char strp2 = ')' then (next_char strp2, Empty)
				      else failwith "Wrong syntax")
    else let head = read_var conv strp1 in
	 if peek_char (fst head) = '(' then ( let strp3 = next_char (fst head) in
					      let (strp4, t1) = bos_helper strp3 in
					      if peek_char strp4 = ',' then ( let strp5 = next_char strp4 in
									      let(strp6, t2) = bos_helper strp5 in
									      if peek_char strp6 = ')' then (next_char strp6, Bin(t1, snd head, t2))
									      else failwith "Wrong syntax" )
					      else failwith "Wrong syntax" )
	 else let pch = peek_char (fst head) in
	      if pch = ',' || pch = ')' then(fst head, Bin(Empty, snd head, Empty))
	      else failwith "Wrong syntax"
  in
  snd (bos_helper {str=str;ptr=0}) ;;

btree_of_string (fun x -> x) "()" ;;

btree_of_string (fun x -> x) "1(2,3)" ;;

btree_of_string (fun x -> x) "1(2,3(4(6,7),5))" ;;

btree_of_string (fun x -> x) "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))" ;;

btree_of_string (fun x -> x) "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))" ;;

btree_of_string int_of_string "1(2((),3((),5)),4(2(6,()),()))" ;;


(* I need a vector font so that transformations work on text as well
 * from http://paulbourke.net/dataformats/hershey/
 *)

(* the two arrays are generated by a program *)

let code_to_sketch = [|
(* char (32)  *)
[ Lift_pen; ] ;
 
(* char (33)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 21.}; {xc= 5.; yc= 7.}; ]]; Lift_pen; Ge [Seg [ {xc= 5.; yc= 2.}; {xc= 4.; yc= 1.}; {xc= 5.; yc= 0.}; {xc= 6.; yc= 1.}; {xc= 5.; yc= 2.}; ]]; Lift_pen; ] ;
 
(* char (34)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 14.}; ]]; Lift_pen; Ge [Seg [ {xc= 12.; yc= 21.}; {xc= 12.; yc= 14.}; ]]; Lift_pen; ] ;
 
(* char (35)  *)
[ Lift_pen; Ge [Seg [ {xc= 11.; yc= 25.}; {xc= 4.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 17.; yc= 25.}; {xc= 10.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 12.}; {xc= 18.; yc= 12.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 6.}; {xc= 17.; yc= 6.}; ]]; Lift_pen; ] ;
 
(* char (36)  *)
[ Lift_pen; Ge [Seg [ {xc= 8.; yc= 25.}; {xc= 8.; yc= -4.}; ]]; Lift_pen; Ge [Seg [ {xc= 12.; yc= 25.}; {xc= 12.; yc= -4.}; ]]; Lift_pen; Ge [Seg [ {xc= 17.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 12.; yc= 21.}; {xc= 8.; yc= 21.}; {xc= 5.; yc= 20.}; {xc= 3.; yc= 18.}; {xc= 3.; yc= 16.}; {xc= 4.; yc= 14.}; {xc= 5.; yc= 13.}; {xc= 7.; yc= 12.}; {xc= 13.; yc= 10.}; {xc= 15.; yc= 9.}; {xc= 16.; yc= 8.}; {xc= 17.; yc= 6.}; {xc= 17.; yc= 3.}; {xc= 15.; yc= 1.}; {xc= 12.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 3.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (37)  *)
[ Lift_pen; Ge [Seg [ {xc= 21.; yc= 21.}; {xc= 3.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 8.; yc= 21.}; {xc= 10.; yc= 19.}; {xc= 10.; yc= 17.}; {xc= 9.; yc= 15.}; {xc= 7.; yc= 14.}; {xc= 5.; yc= 14.}; {xc= 3.; yc= 16.}; {xc= 3.; yc= 18.}; {xc= 4.; yc= 20.}; {xc= 6.; yc= 21.}; {xc= 8.; yc= 21.}; {xc= 10.; yc= 20.}; {xc= 13.; yc= 19.}; {xc= 16.; yc= 19.}; {xc= 19.; yc= 20.}; {xc= 21.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 17.; yc= 7.}; {xc= 15.; yc= 6.}; {xc= 14.; yc= 4.}; {xc= 14.; yc= 2.}; {xc= 16.; yc= 0.}; {xc= 18.; yc= 0.}; {xc= 20.; yc= 1.}; {xc= 21.; yc= 3.}; {xc= 21.; yc= 5.}; {xc= 19.; yc= 7.}; {xc= 17.; yc= 7.}; ]]; Lift_pen; ] ;
 
(* char (38)  *)
[ Lift_pen; Ge [Seg [ {xc= 23.; yc= 12.}; {xc= 23.; yc= 13.}; {xc= 22.; yc= 14.}; {xc= 21.; yc= 14.}; {xc= 20.; yc= 13.}; {xc= 19.; yc= 11.}; {xc= 17.; yc= 6.}; {xc= 15.; yc= 3.}; {xc= 13.; yc= 1.}; {xc= 11.; yc= 0.}; {xc= 7.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 4.; yc= 2.}; {xc= 3.; yc= 4.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 8.}; {xc= 5.; yc= 9.}; {xc= 12.; yc= 13.}; {xc= 13.; yc= 14.}; {xc= 14.; yc= 16.}; {xc= 14.; yc= 18.}; {xc= 13.; yc= 20.}; {xc= 11.; yc= 21.}; {xc= 9.; yc= 20.}; {xc= 8.; yc= 18.}; {xc= 8.; yc= 16.}; {xc= 9.; yc= 13.}; {xc= 11.; yc= 10.}; {xc= 16.; yc= 3.}; {xc= 18.; yc= 1.}; {xc= 20.; yc= 0.}; {xc= 22.; yc= 0.}; {xc= 23.; yc= 1.}; {xc= 23.; yc= 2.}; ]]; Lift_pen; ] ;
 
(* char (39)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 19.}; {xc= 4.; yc= 20.}; {xc= 5.; yc= 21.}; {xc= 6.; yc= 20.}; {xc= 6.; yc= 18.}; {xc= 5.; yc= 16.}; {xc= 4.; yc= 15.}; ]]; Lift_pen; ] ;
 
(* char (40)  *)
[ Lift_pen; Ge [Seg [ {xc= 11.; yc= 25.}; {xc= 9.; yc= 23.}; {xc= 7.; yc= 20.}; {xc= 5.; yc= 16.}; {xc= 4.; yc= 11.}; {xc= 4.; yc= 7.}; {xc= 5.; yc= 2.}; {xc= 7.; yc= -2.}; {xc= 9.; yc= -5.}; {xc= 11.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (41)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 25.}; {xc= 5.; yc= 23.}; {xc= 7.; yc= 20.}; {xc= 9.; yc= 16.}; {xc= 10.; yc= 11.}; {xc= 10.; yc= 7.}; {xc= 9.; yc= 2.}; {xc= 7.; yc= -2.}; {xc= 5.; yc= -5.}; {xc= 3.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (42)  *)
[ Lift_pen; Ge [Seg [ {xc= 8.; yc= 21.}; {xc= 8.; yc= 9.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 18.}; {xc= 13.; yc= 12.}; ]]; Lift_pen; Ge [Seg [ {xc= 13.; yc= 18.}; {xc= 3.; yc= 12.}; ]]; Lift_pen; ] ;
 
(* char (43)  *)
[ Lift_pen; Ge [Seg [ {xc= 13.; yc= 18.}; {xc= 13.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 9.}; {xc= 22.; yc= 9.}; ]]; Lift_pen; ] ;
 
(* char (44)  *)
[ Lift_pen; Ge [Seg [ {xc= 6.; yc= 1.}; {xc= 5.; yc= 0.}; {xc= 4.; yc= 1.}; {xc= 5.; yc= 2.}; {xc= 6.; yc= 1.}; {xc= 6.; yc= -1.}; {xc= 5.; yc= -3.}; {xc= 4.; yc= -4.}; ]]; Lift_pen; ] ;
 
(* char (45)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 9.}; {xc= 22.; yc= 9.}; ]]; Lift_pen; ] ;
 
(* char (46)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 2.}; {xc= 4.; yc= 1.}; {xc= 5.; yc= 0.}; {xc= 6.; yc= 1.}; {xc= 5.; yc= 2.}; ]]; Lift_pen; ] ;
 
(* char (47)  *)
[ Lift_pen; Ge [Seg [ {xc= 20.; yc= 25.}; {xc= 2.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (48)  *)
[ Lift_pen; Ge [Seg [ {xc= 9.; yc= 21.}; {xc= 6.; yc= 20.}; {xc= 4.; yc= 17.}; {xc= 3.; yc= 12.}; {xc= 3.; yc= 9.}; {xc= 4.; yc= 4.}; {xc= 6.; yc= 1.}; {xc= 9.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 14.; yc= 1.}; {xc= 16.; yc= 4.}; {xc= 17.; yc= 9.}; {xc= 17.; yc= 12.}; {xc= 16.; yc= 17.}; {xc= 14.; yc= 20.}; {xc= 11.; yc= 21.}; {xc= 9.; yc= 21.}; ]]; Lift_pen; ] ;
 
(* char (49)  *)
[ Lift_pen; Ge [Seg [ {xc= 6.; yc= 17.}; {xc= 8.; yc= 18.}; {xc= 11.; yc= 21.}; {xc= 11.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (50)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 16.}; {xc= 4.; yc= 17.}; {xc= 5.; yc= 19.}; {xc= 6.; yc= 20.}; {xc= 8.; yc= 21.}; {xc= 12.; yc= 21.}; {xc= 14.; yc= 20.}; {xc= 15.; yc= 19.}; {xc= 16.; yc= 17.}; {xc= 16.; yc= 15.}; {xc= 15.; yc= 13.}; {xc= 13.; yc= 10.}; {xc= 3.; yc= 0.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (51)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 21.}; {xc= 16.; yc= 21.}; {xc= 10.; yc= 13.}; {xc= 13.; yc= 13.}; {xc= 15.; yc= 12.}; {xc= 16.; yc= 11.}; {xc= 17.; yc= 8.}; {xc= 17.; yc= 6.}; {xc= 16.; yc= 3.}; {xc= 14.; yc= 1.}; {xc= 11.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 4.; yc= 2.}; {xc= 3.; yc= 4.}; ]]; Lift_pen; ] ;
 
(* char (52)  *)
[ Lift_pen; Ge [Seg [ {xc= 13.; yc= 21.}; {xc= 3.; yc= 7.}; {xc= 18.; yc= 7.}; ]]; Lift_pen; Ge [Seg [ {xc= 13.; yc= 21.}; {xc= 13.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (53)  *)
[ Lift_pen; Ge [Seg [ {xc= 15.; yc= 21.}; {xc= 5.; yc= 21.}; {xc= 4.; yc= 12.}; {xc= 5.; yc= 13.}; {xc= 8.; yc= 14.}; {xc= 11.; yc= 14.}; {xc= 14.; yc= 13.}; {xc= 16.; yc= 11.}; {xc= 17.; yc= 8.}; {xc= 17.; yc= 6.}; {xc= 16.; yc= 3.}; {xc= 14.; yc= 1.}; {xc= 11.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 4.; yc= 2.}; {xc= 3.; yc= 4.}; ]]; Lift_pen; ] ;
 
(* char (54)  *)
[ Lift_pen; Ge [Seg [ {xc= 16.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 12.; yc= 21.}; {xc= 10.; yc= 21.}; {xc= 7.; yc= 20.}; {xc= 5.; yc= 17.}; {xc= 4.; yc= 12.}; {xc= 4.; yc= 7.}; {xc= 5.; yc= 3.}; {xc= 7.; yc= 1.}; {xc= 10.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 14.; yc= 1.}; {xc= 16.; yc= 3.}; {xc= 17.; yc= 6.}; {xc= 17.; yc= 7.}; {xc= 16.; yc= 10.}; {xc= 14.; yc= 12.}; {xc= 11.; yc= 13.}; {xc= 10.; yc= 13.}; {xc= 7.; yc= 12.}; {xc= 5.; yc= 10.}; {xc= 4.; yc= 7.}; ]]; Lift_pen; ] ;
 
(* char (55)  *)
[ Lift_pen; Ge [Seg [ {xc= 17.; yc= 21.}; {xc= 7.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 21.}; {xc= 17.; yc= 21.}; ]]; Lift_pen; ] ;
 
(* char (56)  *)
[ Lift_pen; Ge [Seg [ {xc= 8.; yc= 21.}; {xc= 5.; yc= 20.}; {xc= 4.; yc= 18.}; {xc= 4.; yc= 16.}; {xc= 5.; yc= 14.}; {xc= 7.; yc= 13.}; {xc= 11.; yc= 12.}; {xc= 14.; yc= 11.}; {xc= 16.; yc= 9.}; {xc= 17.; yc= 7.}; {xc= 17.; yc= 4.}; {xc= 16.; yc= 2.}; {xc= 15.; yc= 1.}; {xc= 12.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 4.; yc= 2.}; {xc= 3.; yc= 4.}; {xc= 3.; yc= 7.}; {xc= 4.; yc= 9.}; {xc= 6.; yc= 11.}; {xc= 9.; yc= 12.}; {xc= 13.; yc= 13.}; {xc= 15.; yc= 14.}; {xc= 16.; yc= 16.}; {xc= 16.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 12.; yc= 21.}; {xc= 8.; yc= 21.}; ]]; Lift_pen; ] ;
 
(* char (57)  *)
[ Lift_pen; Ge [Seg [ {xc= 16.; yc= 14.}; {xc= 15.; yc= 11.}; {xc= 13.; yc= 9.}; {xc= 10.; yc= 8.}; {xc= 9.; yc= 8.}; {xc= 6.; yc= 9.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 14.}; {xc= 3.; yc= 15.}; {xc= 4.; yc= 18.}; {xc= 6.; yc= 20.}; {xc= 9.; yc= 21.}; {xc= 10.; yc= 21.}; {xc= 13.; yc= 20.}; {xc= 15.; yc= 18.}; {xc= 16.; yc= 14.}; {xc= 16.; yc= 9.}; {xc= 15.; yc= 4.}; {xc= 13.; yc= 1.}; {xc= 10.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 4.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (58)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 14.}; {xc= 4.; yc= 13.}; {xc= 5.; yc= 12.}; {xc= 6.; yc= 13.}; {xc= 5.; yc= 14.}; ]]; Lift_pen; Ge [Seg [ {xc= 5.; yc= 2.}; {xc= 4.; yc= 1.}; {xc= 5.; yc= 0.}; {xc= 6.; yc= 1.}; {xc= 5.; yc= 2.}; ]]; Lift_pen; ] ;
 
(* char (59)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 14.}; {xc= 4.; yc= 13.}; {xc= 5.; yc= 12.}; {xc= 6.; yc= 13.}; {xc= 5.; yc= 14.}; ]]; Lift_pen; Ge [Seg [ {xc= 6.; yc= 1.}; {xc= 5.; yc= 0.}; {xc= 4.; yc= 1.}; {xc= 5.; yc= 2.}; {xc= 6.; yc= 1.}; {xc= 6.; yc= -1.}; {xc= 5.; yc= -3.}; {xc= 4.; yc= -4.}; ]]; Lift_pen; ] ;
 
(* char (60)  *)
[ Lift_pen; Ge [Seg [ {xc= 20.; yc= 18.}; {xc= 4.; yc= 9.}; {xc= 20.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (61)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 12.}; {xc= 22.; yc= 12.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 6.}; {xc= 22.; yc= 6.}; ]]; Lift_pen; ] ;
 
(* char (62)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 18.}; {xc= 20.; yc= 9.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (63)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 16.}; {xc= 3.; yc= 17.}; {xc= 4.; yc= 19.}; {xc= 5.; yc= 20.}; {xc= 7.; yc= 21.}; {xc= 11.; yc= 21.}; {xc= 13.; yc= 20.}; {xc= 14.; yc= 19.}; {xc= 15.; yc= 17.}; {xc= 15.; yc= 15.}; {xc= 14.; yc= 13.}; {xc= 13.; yc= 12.}; {xc= 9.; yc= 10.}; {xc= 9.; yc= 7.}; ]]; Lift_pen; Ge [Seg [ {xc= 9.; yc= 2.}; {xc= 8.; yc= 1.}; {xc= 9.; yc= 0.}; {xc= 10.; yc= 1.}; {xc= 9.; yc= 2.}; ]]; Lift_pen; ] ;
 
(* char (64)  *)
[ Lift_pen; Ge [Seg [ {xc= 18.; yc= 13.}; {xc= 17.; yc= 15.}; {xc= 15.; yc= 16.}; {xc= 12.; yc= 16.}; {xc= 10.; yc= 15.}; {xc= 9.; yc= 14.}; {xc= 8.; yc= 11.}; {xc= 8.; yc= 8.}; {xc= 9.; yc= 6.}; {xc= 11.; yc= 5.}; {xc= 14.; yc= 5.}; {xc= 16.; yc= 6.}; {xc= 17.; yc= 8.}; ]]; Lift_pen; Ge [Seg [ {xc= 12.; yc= 16.}; {xc= 10.; yc= 14.}; {xc= 9.; yc= 11.}; {xc= 9.; yc= 8.}; {xc= 10.; yc= 6.}; {xc= 11.; yc= 5.}; ]]; Lift_pen; Ge [Seg [ {xc= 18.; yc= 16.}; {xc= 17.; yc= 8.}; {xc= 17.; yc= 6.}; {xc= 19.; yc= 5.}; {xc= 21.; yc= 5.}; {xc= 23.; yc= 7.}; {xc= 24.; yc= 10.}; {xc= 24.; yc= 12.}; {xc= 23.; yc= 15.}; {xc= 22.; yc= 17.}; {xc= 20.; yc= 19.}; {xc= 18.; yc= 20.}; {xc= 15.; yc= 21.}; {xc= 12.; yc= 21.}; {xc= 9.; yc= 20.}; {xc= 7.; yc= 19.}; {xc= 5.; yc= 17.}; {xc= 4.; yc= 15.}; {xc= 3.; yc= 12.}; {xc= 3.; yc= 9.}; {xc= 4.; yc= 6.}; {xc= 5.; yc= 4.}; {xc= 7.; yc= 2.}; {xc= 9.; yc= 1.}; {xc= 12.; yc= 0.}; {xc= 15.; yc= 0.}; {xc= 18.; yc= 1.}; {xc= 20.; yc= 2.}; {xc= 21.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (65)  *)
[ Lift_pen; Ge [Seg [ {xc= 9.; yc= 21.}; {xc= 1.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 9.; yc= 21.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 7.}; {xc= 14.; yc= 7.}; ]]; Lift_pen; ] ;
 
(* char (66)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 13.; yc= 21.}; {xc= 16.; yc= 20.}; {xc= 17.; yc= 19.}; {xc= 18.; yc= 17.}; {xc= 18.; yc= 15.}; {xc= 17.; yc= 13.}; {xc= 16.; yc= 12.}; {xc= 13.; yc= 11.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 11.}; {xc= 13.; yc= 11.}; {xc= 16.; yc= 10.}; {xc= 17.; yc= 9.}; {xc= 18.; yc= 7.}; {xc= 18.; yc= 4.}; {xc= 17.; yc= 2.}; {xc= 16.; yc= 1.}; {xc= 13.; yc= 0.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (67)  *)
[ Lift_pen; Ge [Seg [ {xc= 18.; yc= 16.}; {xc= 17.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 13.; yc= 21.}; {xc= 9.; yc= 21.}; {xc= 7.; yc= 20.}; {xc= 5.; yc= 18.}; {xc= 4.; yc= 16.}; {xc= 3.; yc= 13.}; {xc= 3.; yc= 8.}; {xc= 4.; yc= 5.}; {xc= 5.; yc= 3.}; {xc= 7.; yc= 1.}; {xc= 9.; yc= 0.}; {xc= 13.; yc= 0.}; {xc= 15.; yc= 1.}; {xc= 17.; yc= 3.}; {xc= 18.; yc= 5.}; ]]; Lift_pen; ] ;
 
(* char (68)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 11.; yc= 21.}; {xc= 14.; yc= 20.}; {xc= 16.; yc= 18.}; {xc= 17.; yc= 16.}; {xc= 18.; yc= 13.}; {xc= 18.; yc= 8.}; {xc= 17.; yc= 5.}; {xc= 16.; yc= 3.}; {xc= 14.; yc= 1.}; {xc= 11.; yc= 0.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (69)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 17.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 11.}; {xc= 12.; yc= 11.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 0.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (70)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 17.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 11.}; {xc= 12.; yc= 11.}; ]]; Lift_pen; ] ;
 
(* char (71)  *)
[ Lift_pen; Ge [Seg [ {xc= 18.; yc= 16.}; {xc= 17.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 13.; yc= 21.}; {xc= 9.; yc= 21.}; {xc= 7.; yc= 20.}; {xc= 5.; yc= 18.}; {xc= 4.; yc= 16.}; {xc= 3.; yc= 13.}; {xc= 3.; yc= 8.}; {xc= 4.; yc= 5.}; {xc= 5.; yc= 3.}; {xc= 7.; yc= 1.}; {xc= 9.; yc= 0.}; {xc= 13.; yc= 0.}; {xc= 15.; yc= 1.}; {xc= 17.; yc= 3.}; {xc= 18.; yc= 5.}; {xc= 18.; yc= 8.}; ]]; Lift_pen; Ge [Seg [ {xc= 13.; yc= 8.}; {xc= 18.; yc= 8.}; ]]; Lift_pen; ] ;
 
(* char (72)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 18.; yc= 21.}; {xc= 18.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 11.}; {xc= 18.; yc= 11.}; ]]; Lift_pen; ] ;
 
(* char (73)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (74)  *)
[ Lift_pen; Ge [Seg [ {xc= 12.; yc= 21.}; {xc= 12.; yc= 5.}; {xc= 11.; yc= 2.}; {xc= 10.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 6.; yc= 0.}; {xc= 4.; yc= 1.}; {xc= 3.; yc= 2.}; {xc= 2.; yc= 5.}; {xc= 2.; yc= 7.}; ]]; Lift_pen; ] ;
 
(* char (75)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 18.; yc= 21.}; {xc= 4.; yc= 7.}; ]]; Lift_pen; Ge [Seg [ {xc= 9.; yc= 12.}; {xc= 18.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (76)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 0.}; {xc= 16.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (77)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 12.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 20.; yc= 21.}; {xc= 12.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 20.; yc= 21.}; {xc= 20.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (78)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 18.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 18.; yc= 21.}; {xc= 18.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (79)  *)
[ Lift_pen; Ge [Seg [ {xc= 9.; yc= 21.}; {xc= 7.; yc= 20.}; {xc= 5.; yc= 18.}; {xc= 4.; yc= 16.}; {xc= 3.; yc= 13.}; {xc= 3.; yc= 8.}; {xc= 4.; yc= 5.}; {xc= 5.; yc= 3.}; {xc= 7.; yc= 1.}; {xc= 9.; yc= 0.}; {xc= 13.; yc= 0.}; {xc= 15.; yc= 1.}; {xc= 17.; yc= 3.}; {xc= 18.; yc= 5.}; {xc= 19.; yc= 8.}; {xc= 19.; yc= 13.}; {xc= 18.; yc= 16.}; {xc= 17.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 13.; yc= 21.}; {xc= 9.; yc= 21.}; ]]; Lift_pen; ] ;
 
(* char (80)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 13.; yc= 21.}; {xc= 16.; yc= 20.}; {xc= 17.; yc= 19.}; {xc= 18.; yc= 17.}; {xc= 18.; yc= 14.}; {xc= 17.; yc= 12.}; {xc= 16.; yc= 11.}; {xc= 13.; yc= 10.}; {xc= 4.; yc= 10.}; ]]; Lift_pen; ] ;
 
(* char (81)  *)
[ Lift_pen; Ge [Seg [ {xc= 9.; yc= 21.}; {xc= 7.; yc= 20.}; {xc= 5.; yc= 18.}; {xc= 4.; yc= 16.}; {xc= 3.; yc= 13.}; {xc= 3.; yc= 8.}; {xc= 4.; yc= 5.}; {xc= 5.; yc= 3.}; {xc= 7.; yc= 1.}; {xc= 9.; yc= 0.}; {xc= 13.; yc= 0.}; {xc= 15.; yc= 1.}; {xc= 17.; yc= 3.}; {xc= 18.; yc= 5.}; {xc= 19.; yc= 8.}; {xc= 19.; yc= 13.}; {xc= 18.; yc= 16.}; {xc= 17.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 13.; yc= 21.}; {xc= 9.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 12.; yc= 4.}; {xc= 18.; yc= -2.}; ]]; Lift_pen; ] ;
 
(* char (82)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 13.; yc= 21.}; {xc= 16.; yc= 20.}; {xc= 17.; yc= 19.}; {xc= 18.; yc= 17.}; {xc= 18.; yc= 15.}; {xc= 17.; yc= 13.}; {xc= 16.; yc= 12.}; {xc= 13.; yc= 11.}; {xc= 4.; yc= 11.}; ]]; Lift_pen; Ge [Seg [ {xc= 11.; yc= 11.}; {xc= 18.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (83)  *)
[ Lift_pen; Ge [Seg [ {xc= 17.; yc= 18.}; {xc= 15.; yc= 20.}; {xc= 12.; yc= 21.}; {xc= 8.; yc= 21.}; {xc= 5.; yc= 20.}; {xc= 3.; yc= 18.}; {xc= 3.; yc= 16.}; {xc= 4.; yc= 14.}; {xc= 5.; yc= 13.}; {xc= 7.; yc= 12.}; {xc= 13.; yc= 10.}; {xc= 15.; yc= 9.}; {xc= 16.; yc= 8.}; {xc= 17.; yc= 6.}; {xc= 17.; yc= 3.}; {xc= 15.; yc= 1.}; {xc= 12.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 5.; yc= 1.}; {xc= 3.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (84)  *)
[ Lift_pen; Ge [Seg [ {xc= 8.; yc= 21.}; {xc= 8.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 1.; yc= 21.}; {xc= 15.; yc= 21.}; ]]; Lift_pen; ] ;
 
(* char (85)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 6.}; {xc= 5.; yc= 3.}; {xc= 7.; yc= 1.}; {xc= 10.; yc= 0.}; {xc= 12.; yc= 0.}; {xc= 15.; yc= 1.}; {xc= 17.; yc= 3.}; {xc= 18.; yc= 6.}; {xc= 18.; yc= 21.}; ]]; Lift_pen; ] ;
 
(* char (86)  *)
[ Lift_pen; Ge [Seg [ {xc= 1.; yc= 21.}; {xc= 9.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 17.; yc= 21.}; {xc= 9.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (87)  *)
[ Lift_pen; Ge [Seg [ {xc= 2.; yc= 21.}; {xc= 7.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 12.; yc= 21.}; {xc= 7.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 12.; yc= 21.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 22.; yc= 21.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (88)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 21.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 17.; yc= 21.}; {xc= 3.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (89)  *)
[ Lift_pen; Ge [Seg [ {xc= 1.; yc= 21.}; {xc= 9.; yc= 11.}; {xc= 9.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 17.; yc= 21.}; {xc= 9.; yc= 11.}; ]]; Lift_pen; ] ;
 
(* char (90)  *)
[ Lift_pen; Ge [Seg [ {xc= 17.; yc= 21.}; {xc= 3.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 21.}; {xc= 17.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 0.}; {xc= 17.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (91)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 25.}; {xc= 4.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 5.; yc= 25.}; {xc= 5.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 25.}; {xc= 11.; yc= 25.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= -7.}; {xc= 11.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (92)  *)
[ Lift_pen; Ge [Seg [ {xc= 0.; yc= 21.}; {xc= 14.; yc= -3.}; ]]; Lift_pen; ] ;
 
(* char (93)  *)
[ Lift_pen; Ge [Seg [ {xc= 9.; yc= 25.}; {xc= 9.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 10.; yc= 25.}; {xc= 10.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 25.}; {xc= 10.; yc= 25.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= -7.}; {xc= 10.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (94)  *)
[ Lift_pen; Ge [Seg [ {xc= 6.; yc= 15.}; {xc= 8.; yc= 18.}; {xc= 10.; yc= 15.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 12.}; {xc= 8.; yc= 17.}; {xc= 13.; yc= 12.}; ]]; Lift_pen; Ge [Seg [ {xc= 8.; yc= 17.}; {xc= 8.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (95)  *)
[ Lift_pen; Ge [Seg [ {xc= 0.; yc= -2.}; {xc= 16.; yc= -2.}; ]]; Lift_pen; ] ;
 
(* char (96)  *)
[ Lift_pen; Ge [Seg [ {xc= 6.; yc= 21.}; {xc= 5.; yc= 20.}; {xc= 4.; yc= 18.}; {xc= 4.; yc= 16.}; {xc= 5.; yc= 15.}; {xc= 6.; yc= 16.}; {xc= 5.; yc= 17.}; ]]; Lift_pen; ] ;
 
(* char (97)  *)
[ Lift_pen; Ge [Seg [ {xc= 15.; yc= 14.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 15.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (98)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 11.}; {xc= 6.; yc= 13.}; {xc= 8.; yc= 14.}; {xc= 11.; yc= 14.}; {xc= 13.; yc= 13.}; {xc= 15.; yc= 11.}; {xc= 16.; yc= 8.}; {xc= 16.; yc= 6.}; {xc= 15.; yc= 3.}; {xc= 13.; yc= 1.}; {xc= 11.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 6.; yc= 1.}; {xc= 4.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (99)  *)
[ Lift_pen; Ge [Seg [ {xc= 15.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (100)  *)
[ Lift_pen; Ge [Seg [ {xc= 15.; yc= 21.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 15.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (101)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 8.}; {xc= 15.; yc= 8.}; {xc= 15.; yc= 10.}; {xc= 14.; yc= 12.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (102)  *)
[ Lift_pen; Ge [Seg [ {xc= 10.; yc= 21.}; {xc= 8.; yc= 21.}; {xc= 6.; yc= 20.}; {xc= 5.; yc= 17.}; {xc= 5.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 2.; yc= 14.}; {xc= 9.; yc= 14.}; ]]; Lift_pen; ] ;
 
(* char (103)  *)
[ Lift_pen; Ge [Seg [ {xc= 15.; yc= 14.}; {xc= 15.; yc= -2.}; {xc= 14.; yc= -5.}; {xc= 13.; yc= -6.}; {xc= 11.; yc= -7.}; {xc= 8.; yc= -7.}; {xc= 6.; yc= -6.}; ]]; Lift_pen; Ge [Seg [ {xc= 15.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (104)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 10.}; {xc= 7.; yc= 13.}; {xc= 9.; yc= 14.}; {xc= 12.; yc= 14.}; {xc= 14.; yc= 13.}; {xc= 15.; yc= 10.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (105)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 21.}; {xc= 4.; yc= 20.}; {xc= 5.; yc= 21.}; {xc= 4.; yc= 22.}; {xc= 3.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 14.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (106)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 21.}; {xc= 6.; yc= 20.}; {xc= 7.; yc= 21.}; {xc= 6.; yc= 22.}; {xc= 5.; yc= 21.}; ]]; Lift_pen; Ge [Seg [ {xc= 6.; yc= 14.}; {xc= 6.; yc= -3.}; {xc= 5.; yc= -6.}; {xc= 3.; yc= -7.}; {xc= 1.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (107)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 14.; yc= 14.}; {xc= 4.; yc= 4.}; ]]; Lift_pen; Ge [Seg [ {xc= 8.; yc= 8.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (108)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 21.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (109)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 14.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 10.}; {xc= 7.; yc= 13.}; {xc= 9.; yc= 14.}; {xc= 12.; yc= 14.}; {xc= 14.; yc= 13.}; {xc= 15.; yc= 10.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 15.; yc= 10.}; {xc= 18.; yc= 13.}; {xc= 20.; yc= 14.}; {xc= 23.; yc= 14.}; {xc= 25.; yc= 13.}; {xc= 26.; yc= 10.}; {xc= 26.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (110)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 14.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 10.}; {xc= 7.; yc= 13.}; {xc= 9.; yc= 14.}; {xc= 12.; yc= 14.}; {xc= 14.; yc= 13.}; {xc= 15.; yc= 10.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (111)  *)
[ Lift_pen; Ge [Seg [ {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; {xc= 16.; yc= 6.}; {xc= 16.; yc= 8.}; {xc= 15.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; ]]; Lift_pen; ] ;
 
(* char (112)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 14.}; {xc= 4.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 11.}; {xc= 6.; yc= 13.}; {xc= 8.; yc= 14.}; {xc= 11.; yc= 14.}; {xc= 13.; yc= 13.}; {xc= 15.; yc= 11.}; {xc= 16.; yc= 8.}; {xc= 16.; yc= 6.}; {xc= 15.; yc= 3.}; {xc= 13.; yc= 1.}; {xc= 11.; yc= 0.}; {xc= 8.; yc= 0.}; {xc= 6.; yc= 1.}; {xc= 4.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (113)  *)
[ Lift_pen; Ge [Seg [ {xc= 15.; yc= 14.}; {xc= 15.; yc= -7.}; ]]; Lift_pen; Ge [Seg [ {xc= 15.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 11.; yc= 14.}; {xc= 8.; yc= 14.}; {xc= 6.; yc= 13.}; {xc= 4.; yc= 11.}; {xc= 3.; yc= 8.}; {xc= 3.; yc= 6.}; {xc= 4.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 11.; yc= 0.}; {xc= 13.; yc= 1.}; {xc= 15.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (114)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 14.}; {xc= 4.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 4.; yc= 8.}; {xc= 5.; yc= 11.}; {xc= 7.; yc= 13.}; {xc= 9.; yc= 14.}; {xc= 12.; yc= 14.}; ]]; Lift_pen; ] ;
 
(* char (115)  *)
[ Lift_pen; Ge [Seg [ {xc= 14.; yc= 11.}; {xc= 13.; yc= 13.}; {xc= 10.; yc= 14.}; {xc= 7.; yc= 14.}; {xc= 4.; yc= 13.}; {xc= 3.; yc= 11.}; {xc= 4.; yc= 9.}; {xc= 6.; yc= 8.}; {xc= 11.; yc= 7.}; {xc= 13.; yc= 6.}; {xc= 14.; yc= 4.}; {xc= 14.; yc= 3.}; {xc= 13.; yc= 1.}; {xc= 10.; yc= 0.}; {xc= 7.; yc= 0.}; {xc= 4.; yc= 1.}; {xc= 3.; yc= 3.}; ]]; Lift_pen; ] ;
 
(* char (116)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 21.}; {xc= 5.; yc= 4.}; {xc= 6.; yc= 1.}; {xc= 8.; yc= 0.}; {xc= 10.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 2.; yc= 14.}; {xc= 9.; yc= 14.}; ]]; Lift_pen; ] ;
 
(* char (117)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 14.}; {xc= 4.; yc= 4.}; {xc= 5.; yc= 1.}; {xc= 7.; yc= 0.}; {xc= 10.; yc= 0.}; {xc= 12.; yc= 1.}; {xc= 15.; yc= 4.}; ]]; Lift_pen; Ge [Seg [ {xc= 15.; yc= 14.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (118)  *)
[ Lift_pen; Ge [Seg [ {xc= 2.; yc= 14.}; {xc= 8.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 14.; yc= 14.}; {xc= 8.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (119)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 14.}; {xc= 7.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 11.; yc= 14.}; {xc= 7.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 11.; yc= 14.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 19.; yc= 14.}; {xc= 15.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (120)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 14.}; {xc= 14.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 14.; yc= 14.}; {xc= 3.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (121)  *)
[ Lift_pen; Ge [Seg [ {xc= 2.; yc= 14.}; {xc= 8.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 14.; yc= 14.}; {xc= 8.; yc= 0.}; {xc= 6.; yc= -4.}; {xc= 4.; yc= -6.}; {xc= 2.; yc= -7.}; {xc= 1.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (122)  *)
[ Lift_pen; Ge [Seg [ {xc= 14.; yc= 14.}; {xc= 3.; yc= 0.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 14.}; {xc= 14.; yc= 14.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 0.}; {xc= 14.; yc= 0.}; ]]; Lift_pen; ] ;
 
(* char (123)  *)
[ Lift_pen; Ge [Seg [ {xc= 9.; yc= 25.}; {xc= 7.; yc= 24.}; {xc= 6.; yc= 23.}; {xc= 5.; yc= 21.}; {xc= 5.; yc= 19.}; {xc= 6.; yc= 17.}; {xc= 7.; yc= 16.}; {xc= 8.; yc= 14.}; {xc= 8.; yc= 12.}; {xc= 6.; yc= 10.}; ]]; Lift_pen; Ge [Seg [ {xc= 7.; yc= 24.}; {xc= 6.; yc= 22.}; {xc= 6.; yc= 20.}; {xc= 7.; yc= 18.}; {xc= 8.; yc= 17.}; {xc= 9.; yc= 15.}; {xc= 9.; yc= 13.}; {xc= 8.; yc= 11.}; {xc= 4.; yc= 9.}; {xc= 8.; yc= 7.}; {xc= 9.; yc= 5.}; {xc= 9.; yc= 3.}; {xc= 8.; yc= 1.}; {xc= 7.; yc= 0.}; {xc= 6.; yc= -2.}; {xc= 6.; yc= -4.}; {xc= 7.; yc= -6.}; ]]; Lift_pen; Ge [Seg [ {xc= 6.; yc= 8.}; {xc= 8.; yc= 6.}; {xc= 8.; yc= 4.}; {xc= 7.; yc= 2.}; {xc= 6.; yc= 1.}; {xc= 5.; yc= -1.}; {xc= 5.; yc= -3.}; {xc= 6.; yc= -5.}; {xc= 7.; yc= -6.}; {xc= 9.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (124)  *)
[ Lift_pen; Ge [Seg [ {xc= 4.; yc= 25.}; {xc= 4.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (125)  *)
[ Lift_pen; Ge [Seg [ {xc= 5.; yc= 25.}; {xc= 7.; yc= 24.}; {xc= 8.; yc= 23.}; {xc= 9.; yc= 21.}; {xc= 9.; yc= 19.}; {xc= 8.; yc= 17.}; {xc= 7.; yc= 16.}; {xc= 6.; yc= 14.}; {xc= 6.; yc= 12.}; {xc= 8.; yc= 10.}; ]]; Lift_pen; Ge [Seg [ {xc= 7.; yc= 24.}; {xc= 8.; yc= 22.}; {xc= 8.; yc= 20.}; {xc= 7.; yc= 18.}; {xc= 6.; yc= 17.}; {xc= 5.; yc= 15.}; {xc= 5.; yc= 13.}; {xc= 6.; yc= 11.}; {xc= 10.; yc= 9.}; {xc= 6.; yc= 7.}; {xc= 5.; yc= 5.}; {xc= 5.; yc= 3.}; {xc= 6.; yc= 1.}; {xc= 7.; yc= 0.}; {xc= 8.; yc= -2.}; {xc= 8.; yc= -4.}; {xc= 7.; yc= -6.}; ]]; Lift_pen; Ge [Seg [ {xc= 8.; yc= 8.}; {xc= 6.; yc= 6.}; {xc= 6.; yc= 4.}; {xc= 7.; yc= 2.}; {xc= 8.; yc= 1.}; {xc= 9.; yc= -1.}; {xc= 9.; yc= -3.}; {xc= 8.; yc= -5.}; {xc= 7.; yc= -6.}; {xc= 5.; yc= -7.}; ]]; Lift_pen; ] ;
 
(* char (126)  *)
[ Lift_pen; Ge [Seg [ {xc= 3.; yc= 6.}; {xc= 3.; yc= 8.}; {xc= 4.; yc= 11.}; {xc= 6.; yc= 12.}; {xc= 8.; yc= 12.}; {xc= 10.; yc= 11.}; {xc= 14.; yc= 8.}; {xc= 16.; yc= 7.}; {xc= 18.; yc= 7.}; {xc= 20.; yc= 8.}; {xc= 21.; yc= 10.}; ]]; Lift_pen; Ge [Seg [ {xc= 3.; yc= 8.}; {xc= 4.; yc= 10.}; {xc= 6.; yc= 11.}; {xc= 8.; yc= 11.}; {xc= 10.; yc= 10.}; {xc= 14.; yc= 7.}; {xc= 16.; yc= 6.}; {xc= 18.; yc= 6.}; {xc= 20.; yc= 7.}; {xc= 21.; yc= 10.}; {xc= 21.; yc= 12.}; ]]; Lift_pen; ] ;
 |];;

let code_width  = [| 16. ; 10. ; 16. ; 21. ; 20. ; 24. ; 26. ; 10. ; 14. ; 14. ; 16. ; 26. ; 10. ; 26. ; 10. ; 22. ; 20. ; 20. ; 20. ; 20. ; 20. ; 20. ; 20. ; 20. ; 20. ; 20. ; 10. ; 10. ; 24. ; 26. ; 24. ; 18. ; 27. ; 18. ; 21. ; 21. ; 21. ; 19. ; 18. ; 21. ; 22. ; 8. ; 16. ; 21. ; 17. ; 24. ; 22. ; 22. ; 21. ; 22. ; 21. ; 20. ; 16. ; 22. ; 18. ; 24. ; 20. ; 18. ; 20. ; 14. ; 14. ; 14. ; 16. ; 16. ; 10. ; 19. ; 19. ; 18. ; 19. ; 18. ; 12. ; 19. ; 19. ; 8. ; 10. ; 17. ; 8. ; 30. ; 19. ; 19. ; 19. ; 19. ; 13. ; 17. ; 12. ; 19. ; 16. ; 22. ; 17. ; 16. ; 17. ; 14. ; 8. ; 14. ; 24. ; |];;

(* why are for loops not indented ? *)
let make_text_picture size linewidth color str =
  let xstart = ref 0.0 in
  let sketchl = ref [] in
  for i = 0 to (String.length str)-1 do
  let ch = String.get str i in
  let code = Char.code(ch) - 32 in
  if code < 0 || code > (Array.length code_to_sketch) -1
  then failwith "Unkown character in make_text_picture"
  else ( sketchl := !sketchl @  [ transform_sketch  (compose_transformations [ scaling (size, size) ;
									       (translation !xstart 0.0) ] )
  						    code_to_sketch.(code) ] ;
	 xstart := !xstart +. code_width.(code) )
  done ;
  make_picture (linewidth, color)
	       (group_sketches !sketchl) ;;

(* the following two functions are from chapter 6 *)
       
let rec btree_hom f v t =
  match t with
    Bin (t1,a,t2) -> f (btree_hom f v t1, a, btree_hom  f v t2)
  | Empty -> v ;;
 
let map_btree f t =
  btree_hom (fun (t1,a,t2) -> Bin(t1, f a, t2))
	    Empty t ;;

(* simplified compared to the book *)
type tree_style =
    { vdist: float;
      hdist: float;
      coef_list: float list;
      tlinewidth: float;
      tcolor: color} ;;

let draw_btree tsty t =
  let rec drawr d cl ({xc=x; yc=y} as pt) = function
    Empty -> make_blank_picture 0.0 0.0
    | Bin (Empty, pict, Empty) -> center_picture pict pt
    | Bin(t1, pict, t2) -> let d = d *. (List.hd cl) in
			   let pt1 = {xc = x -. d/. 2.0; yc = y -. tsty.vdist} in
			   let pt2 = {xc = x +. d/. 2.0; yc = y -. tsty.vdist} in
			   let line1 = make_picture (tsty.tlinewidth, tsty.tcolor) (make_sketch [Seg [pt; pt1]]) in
			   let line2 = make_picture (tsty.tlinewidth, tsty.tcolor) (make_sketch [Seg [pt; pt2]]) in
			   match (t1,t2) with
			     (_, Empty) -> group_pictures [line1; center_picture pict pt; drawr d (List.tl cl) pt1 t1]
			   | (Empty, _) -> group_pictures [line2; center_picture pict pt; drawr d (List.tl cl) pt2 t2]
			   | _ -> group_pictures [line1; line2; center_picture pict pt; drawr d (List.tl cl) pt1 t1;
						  drawr d (List.tl cl) pt2 t2]
  in
  drawr tsty.hdist tsty.coef_list origin t ;;

let black = {r=0;g=0;b=0} ;;
let white = {r=255;g=255;b=255} ;;
let red = {r=255;g=0;b=0} ;;
let blue = {r=0;g=0;b=255} ;;

let p1 =
  let t1 = btree_of_string int_of_string "1(2,3(4(6,7),5))" in
  let linewidth = 1.0 in
  let cl1 = [1.0; 1.0; 1.0] in
  let tstyle1 = {vdist = 50.0; hdist= 50.0; coef_list = cl1; tlinewidth = linewidth; tcolor=white } in
  draw_btree tstyle1
	     (map_btree (fun x -> make_blank_picture 0.0 0.0) t1) ;;

let p2 =
  let t2 = btree_of_string int_of_string "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))" in
  let linewidth = 1.0 in
  let cl2 = [1.0; 0.5; 0.5] in
  let tstyle2 = {vdist = 50.0; hdist= 100.0; coef_list = cl2; tlinewidth = linewidth; tcolor=red } in
  draw_btree tstyle2
	     (map_btree (fun x -> make_blank_picture 0.0 0.0) t2) ;;

let draw_string_node r a =
  let s = center_picture (make_text_picture 0.5 1.0 red a)
			 origin in
  let c = make_picture (1.0, blue)
		       (make_sketch [ Arc (origin, r, 0.0, 360.0)]) in
  group_pictures [c; s] ;;

let draw_int_node r n = draw_string_node r (string_of_int n) ;;

let p1 =
  let t1 = btree_of_string int_of_string "1(2,3(4(6,7),5))" in
  let linewidth = 1.0 in
  let cl1 = [1.0; 1.0; 1.0] in
  let tstyle1 = {vdist = 50.0; hdist= 50.0; coef_list = cl1; tlinewidth = linewidth; tcolor=white } in
  draw_btree tstyle1
	     (map_btree (fun x -> draw_int_node 10.0 x) t1) ;;

let p2 =
  let t2 = btree_of_string int_of_string "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))" in
  let linewidth = 1.0 in
  let cl2 = [1.0; 0.5; 0.5] in
  let tstyle2 = {vdist = 50.0; hdist= 100.0; coef_list = cl2; tlinewidth = linewidth; tcolor=red } in
  draw_btree tstyle2
	     (map_btree (fun x -> draw_int_node 10.0 x) t2) ;;

let puts str =
  print_string str ;
  print_newline () ;;

let putsf floatv =
  puts (string_of_float floatv);;

let window_flags = 47 ;;

(* can be changed in the UI *)
let scale_factor_x = ref 50 ;;
let scale_factor_y = ref 50 ;;

type pen_state = Up
	       | Down ;;

(* TODO : just add the grid data to the picture to be drawn with group_* *)

let draw_grid canvas transf xmin xmax ymin ymax =
  let steps = range_values_step xmin xmax 1.0 in
  let gray1 = {r=150;g=150;b=150} in
  let gray2 = {r=20;g=20;b=20} in
  List.iter (fun x -> let pt1 = {xc=x; yc= ymin} in
		      let pt1'= transform_point transf pt1 in
		      let pt2 = {xc=x; yc= ymax} in
		      let pt2'= transform_point transf pt2 in
		      line canvas 0.5 gray1 pt1' pt2')
	    steps;
  List.iter (fun x -> let pt1 = {xc= xmin; yc= x} in
		      let pt1'= transform_point transf pt1 in
		      let pt2 = {xc= xmax; yc=x} in
		      let pt2'= transform_point transf pt2 in		      
		      line canvas 0.5 gray1 pt1' pt2')
	    steps;
  (* draw x and y axis *)
  let y1 = transform_point transf {xc=0.0; yc=ymin} in 
  let y2 = transform_point transf {xc=0.0; yc=ymax} in 
  let x1 = transform_point transf {xc=xmin; yc=0.0} in 
  let x2 = transform_point transf {xc=xmax; yc=0.0} in 
  line canvas 3.0 gray2 y1 y2 ;
  line canvas 3.0 gray2 x1 x2 ;;

let draw_sketch ctx lw color sk =
  let pen = ref Up in
  let (xcur, ycur) = (ref 0.0, ref 0.0) in
  let canvas = get_canvas ctx in
  List.iter (fun s -> match s with
			Lift_pen -> pen := Up
		      | Ge g -> List.iter (fun g -> match g with
						      Seg pts -> List.iter (fun pt -> if !pen = Up
										      then begin
										      pen := Down;
										      xcur := pt.xc;
										      ycur := pt.yc
										      end
										      else begin
										      line canvas lw color {xc = !xcur; yc = !ycur} pt;
										      xcur := pt.xc;
										      ycur := pt.yc
										      end
									   )
									   pts
						    | Arc (pt, r, amin, amax) -> failwith "I don't expect an Arc here.:"
						    | Curve (a,c1,c2,b) -> curve canvas lw color a c1 c2 b)
					  g)
	    sk ;;

let draw_picture ctx pic (xmin,xmax) (ymin,ymax) (xcenter, ycenter) =
  let canvas = get_canvas ctx in
  let reg = get_region ctx in
  let (w, h, x, y) = (reg.w, reg.h, reg.x, reg.y) in
  let transf = compose_transformations [ translation (x +. w/.2.0) (y +. h/.2.0) ;
					 scaling (float_of_int !scale_factor_x, float_of_int !scale_factor_y);
					 xaxis_symmetry;
					 translation (-. xcenter) (-. ycenter) ]	 
  in
  draw_grid canvas transf xmin xmax ymin ymax ;
  let pic' = transform_picture transf pic in
  List.iter (fun sub_pic -> draw_sketch ctx sub_pic.linewidth sub_pic.color sub_pic.sketch) pic' ;;

let contents =
  fun ctx ->
    if gui_begin ctx "Main" 50.0 50.0 250.0 250.0 window_flags > 0 then
      begin
	layout_row_static ctx 20.0 200 1;
	label ctx "Chapter 9" 17;
	layout_row_static ctx 20.0 200 1;
	if button_label ctx "Back to defaults" > 0 then
	begin
	scale_factor_x := 50 ;
	scale_factor_y := 50 ;
	arc_to_segs_n := 20 ;
	end;
	layout_row_static ctx 20.0 200 1;
	slider_pct ctx "Scale factor x:" scale_factor_x 1 1.0 ;
	layout_row_static ctx 20.0 200 1;
	slider_pct ctx "Scale factor y:" scale_factor_y 1 1.0 ;
	layout_row_static ctx 20.0 200 1;
	slider_pct ctx "Arc to segs:" arc_to_segs_n 1 1.0 ;
      end ;
    gui_end ctx ;;

let geometric_elements =
  fun ctx ->
    if gui_begin ctx "9.1.2 Geometric Elements" 50.0 350.0 550.0 650.0 window_flags > 0 then
    let pic = 
      let ptA = {xc= -. 3.0 ; yc= -. 3.0 } in
      let ptB = {xc= -. 3.0 ; yc= -. 1.0 } in
      let ptC = {xc= -. 1.0 ; yc= -. 1.0 } in
      let ptD = {xc= -. 1.0 ; yc= -. 3.0 } in
      let ptE = {xc= -. 3.0 ; yc=    4.0 } in     
      let ptF = {xc= -. 2.0 ; yc=    0.0 } in
      let ptG = {xc=    0.0 ; yc=    5.0 } in
      let ptH = {xc=    1.0 ; yc=    4.0 } in
      let ptI = {xc=    3.0 ; yc=    0.0 } in
      make_picture (0.3, {r=0;g=200;b=200})
		   (group_sketches [ (make_sketch [ Seg [ ptA; ptB; ptC; ptD; ptA] ]) ;
				     (make_sketch [ Curve ( ptE, ptF, ptG, ptH) ]) ;
				     (make_sketch [ Arc ( ptI, 2.0, 30.0, 290.0 ) ] ) ])
      in
      draw_picture ctx pic (-. 5.0, 5.0) (-. 5.0, 5.0) (0.0,0.0);
      gui_end ctx ;;

(* ! BUG in the book, the end parens in figure 9.3 are wrong at the end of each make_sketch *)

let constructing_images =
  fun ctx ->
  if gui_begin ctx "9.1.3 Constructing Images" 650.0 350.0 550.0 650.0 window_flags > 0 then
  let pic = group_pictures [ (make_picture (1.0, {r=0; g=250;b=250})
					   (make_sketch [Arc ({xc=5.0; yc=7.0}, 2.0, -90.0, 90.0);
							 Seg[{xc=5.0; yc=9.0}; {xc=3.0; yc=9.0};
							     {xc=3.0; yc=1.0}; {xc=4.0;yc=1.0};
							     {xc=4.0;yc=5.0}; {xc=5.0;yc=5.0}]]));
			     (make_picture (1.0, {r=0; g=250;b=250})
					   (make_sketch [Arc({xc=5.0; yc=7.0}, 1.0, -90.0, 90.0) ;
							 Seg [{xc=5.0;yc=8.0}; {xc=4.0;yc=8.0};
							      {xc=4.0;yc=6.0}; {xc=5.0;yc=6.0}]])) ;
			     (center_picture (make_text_picture 0.02 1.0 {r=134;g=156;b=178} "another test")
					     {xc = 4.0 ; yc = 5.0} )]
  in
  draw_picture ctx pic ( 0.0, 10.0) ( 0.0, 10.0) (4.0,5.0) ;
  gui_end ctx ;;

let drawing_trees =
  fun ctx ->
  if gui_begin ctx "9.2.1 Drawing principles" 750.0 450.0 550.0 650.0 window_flags > 0 then
  let pic = group_pictures [ (center_picture (transform_picture (scaling ( 0.04, 0.04))
								(let t1 = btree_of_string int_of_string "1(2,3(4(6,7),5))" in
								 let linewidth = 1.0 in
								 let cl1 = [1.0; 1.0; 1.0] in
								 let tstyle1 = {vdist = 50.0; hdist= 50.0; coef_list = cl1; tlinewidth = linewidth; tcolor=white } in
								 draw_btree tstyle1
									    (map_btree (fun x -> draw_int_node 10.0 x) t1)))
					     {xc= 6.0; yc=5.0}) ;
			     (center_picture (transform_picture (scaling ( 0.04, 0.04))
								(let t2 = btree_of_string int_of_string "1(2(4(8,9),5(10,11)),3(6(12,13),7(14,15)))" in
								 let linewidth = 2.0 in
								 let cl2 = [1.0; 0.5; 0.5] in
								 let tstyle2 = {vdist = 50.0; hdist= 100.0; coef_list = cl2; tlinewidth = linewidth; tcolor=red } in
								 draw_btree tstyle2
									    (map_btree (fun x -> draw_int_node 10.0 x) t2)))
								{xc= 3.0; yc=2.0})]
  in
  draw_picture ctx pic (-. 1.0, 10.0) (-. 1.0, 10.0) (4.0, 5.0) ;
  gui_end ctx ;;

let ocaml_gui =
  fun ctx ->
  contents ctx;
  geometric_elements ctx ;
  constructing_images ctx ;
  drawing_trees ctx ;;

let () =
  Callback.register "ocaml_gui" ocaml_gui ;;

