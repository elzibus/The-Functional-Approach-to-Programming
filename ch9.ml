
(* 9.1.1 Geometric Plane *)

type point = {xc: float; yc: float } ;;

let origin = {xc= 0.0; yc= 0.0} ;;

(* 9.1.2 Geometric elements *)

type geom_element =
    Seg of point list
  | Arc of point * float * float * float
  | Curve of point * point * point * point ;;

let sk1 = 
  let ptA = {xc= -. 3.0 ; yc= -. 3.0 } in
  let ptB = {xc= -. 3.0 ; yc= -. 1.0 } in
  let ptC = {xc= -. 1.0 ; yc= -. 1.0 } in
  let ptD = {xc= -. 1.0 ; yc= -. 3.0 } in
  let ptE = {xc= -. 3.0 ; yc= -. 4.0 } in
  let ptF = {xc= -. 2.0 ; yc=    0.0 } in
  let ptG = {xc=    0.0 ; yc=    5.0 } in
  let ptH = {xc=    1.0 ; yc=    4.0 } in
  let ptI = {xc=    3.0 ; yc=    0.0 } in
  [ Seg [ ptA; ptB; ptC; ptD; ptA] ;
    Curve ( ptE, ptF, ptG, ptH) ;
    Arc ( ptI, 2.0, 30.0, 90.0 ) ] ;;

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
