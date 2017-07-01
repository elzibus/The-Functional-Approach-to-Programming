(* exercise 9.1 *)

let pi = 4.0 *. atan 1.0;;

let deg_to_rad deg = deg *. pi /. 180.0 ;;

let rad_to_deg rad = (180.0 *. rad) /. pi ;;

let sin_deg deg = sin( deg_to_rad deg ) ;;

let cos_deg deg = cos( deg_to_rad deg ) ;;

(* Exercise 9.2 *)

let translation tx ty = {m11=1.0; m12=0.0; m13=tx;
			 m21=0.0; m22=1.0; m23=ty} ;;

transform_point (translation 1.0 2.0)
		{xc=123.0 ; yc=456.0} ;;

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
