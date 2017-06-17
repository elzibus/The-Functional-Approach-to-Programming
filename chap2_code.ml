
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

(* ! no "==" in Ocaml *)
  
type intpair = int * int ;;
  
type intpair = Intpair of int * int ;;

  
