
(* This chapter 9 code is to be compiled and uses nuklear for the graphics *)

(* 9.1.1 Geometric Plane *)

type point = { xc: float; yc: float } ;;

let origin = {xc= 0.0; yc= 0.0} ;;

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

let group_sketches (sl:sketch list) =
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

(* "picture" yet to be introduced
 * the center of a Seg is the barycenter of all points that compose the Seg
 * the center of an Arc is its center
 * the center of a Curve is the barycenter of its constituting points
 *)

let plist_center pl =
  let rec sc_helper l x y n =
    match l with
      [] -> {xc = x /. n; yc = y/. n}
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

let center_sketch sk pt =
  let temp = List.map (fun skp -> match skp with
				    Lift_pen -> []
				  | Ge ge -> List.map (fun elt -> match elt with
								    Seg sl -> plist_center sl
								  | Arc (p,_,_,_) -> p
								  | Curve (p1,p2,p3,p4) -> fpoint_center (p1,p2,p3,p4))
						      ge)
		      sk
  in
  let center = plist_center ( List.flatten temp) in
  transform_sketch (translation center.xc center.yc) sk ;;

(* I am not using the line style parameter *)

type tree_style =
    { vdist: float;
      hdist: float;
      coef_list: float list} ;;

type 'a btree = Empty
	      | Bin of 'a btree * 'a * 'a btree ;;

let draw_btree tsty t =
  let rec drawr d cl ({xc=x; yc=y} as pt) = function
    Empty -> []
    | (Bin (Empty, pict, Empty)) -> center_sketch pict pt
    | (Bin(t1, pict, t2)) -> let d = d *. (List.hd cl) in
			     let pt1 = {xc = x -. d/. 2.0; yc = y -. tsty.vdist} in
			     let pt2 = {xc = x +. d/. 2.0; yc = y -. tsty.vdist} in
			     let line1 = make_sketch [Seg [pt; pt1]] in
			     let line2 = make_sketch [Seg [pt; pt2]] in
			     match (t1,t2) with
				   (_, Empty) -> group_sketches [line1; center_sketch pict pt; drawr d (List.tl cl) pt1 t1]
				 | (Empty, _) -> group_sketches [line2; center_sketch pict pt; drawr d (List.tl cl) pt2 t2]
				 | _ -> group_sketches [line1; line2; center_sketch pict pt; drawr d (List.tl cl) pt1 t1;
							drawr d (List.tl cl) pt2 t2]
  in
  drawr tsty.hdist tsty.coef_list origin t ;;

(* following functions are from chapter 6 *)

let rec btree_hom f v t =
  match t with
    Bin (t1,a,t2) -> f (btree_hom f v t1, a, btree_hom  f v t2)
  | Empty -> v ;;
 
let map_btree f t =
  btree_hom (fun (t1,a,t2) -> Bin(t1, f a, t2))
	    Empty t ;;

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
  
(* ----------------------------- *)

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

let draw_sketch ctx sk (xmin,xmax) (ymin,ymax) (xcenter, ycenter) =
  let pen = ref Up in
  let (xcur, ycur) = (ref 0.0, ref 0.0) in
  let canvas = get_canvas ctx in
  let reg = get_region ctx in
  let (w, h, x, y) = (reg.w, reg.h, reg.x, reg.y) in
  let transf = compose_transformations [ translation (x +. w/.2.0) (y +. h/.2.0) ;
					 scaling (float_of_int !scale_factor_x, float_of_int !scale_factor_y);
					 xaxis_symmetry;
					 translation (-. xcenter) (-. ycenter) ]	 
  in
  draw_grid canvas transf xmin xmax ymin ymax ;
  let sk' = transform_sketch transf sk in
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
										      line canvas 4.0 {r=200;g=0;b=0} {xc = !xcur; yc = !ycur} pt;
										      xcur := pt.xc;
										      ycur := pt.yc
										      end
									   )
									   pts
						    | Arc (pt, r, amin, amax) -> failwith "I don't expect an Arc here.:"
						    | Curve (a,c1,c2,b) -> curve canvas 4.0 {r=0;g=0;b=100} a c1 c2 b)
					  g)
	    sk' ;;

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
		       (make_sketch [ Arc ( ptI, 2.0, 30.0, 290.0 ) ] ) ] in
    draw_sketch ctx sk1 (-. 5.0, 5.0) (-. 5.0, 5.0) (0.0,0.0);
    gui_end ctx ;;

let constructing_images =
  fun ctx ->
    if gui_begin ctx "9.1.3 Constructing Images" 650.0 350.0 550.0 650.0 window_flags > 0 then
    let skp = group_sketches [ (make_sketch
				[Arc ({xc=5.0; yc=7.0}, 2.0, -90.0, 90.0);
				 Seg[{xc=5.0; yc=9.0}; {xc=3.0; yc=9.0};
				     {xc=3.0; yc=1.0}; {xc=4.0;yc=1.0};
				     {xc=4.0;yc=5.0}; {xc=5.0;yc=5.0}]]); (* BUG: this paren soup is incorrect in the book *)
			       (make_sketch
				[Arc({xc=5.0; yc=7.0}, 1.0, -90.0, 90.0) ;
				 Seg [{xc=5.0;yc=8.0}; {xc=4.0;yc=8.0};
				      {xc=4.0;yc=6.0}; {xc=5.0;yc=6.0}]])] in  (* BUG: this paren soup is incorrect in the book *)
    draw_sketch ctx skp (-. 1.0, 10.0) (-. 1.0, 10.0) (5.0,5.0) ;
    gui_end ctx ;;

let ocaml_gui =
  fun ctx ->
  contents ctx;
  geometric_elements ctx ;
  constructing_images ctx ;;

let () =
  Callback.register "ocaml_gui" ocaml_gui ;;
