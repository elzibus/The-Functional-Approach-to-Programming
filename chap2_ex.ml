
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

