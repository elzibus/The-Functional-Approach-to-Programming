
(* exercise 1.1 *)

let has_solutions a b c = b *. b -. 4. *. a *. c >= 0. ;;

(* exercise 1.2 *)

let derivative f dx = fun x -> (f(x +. dx) -. f(x)) /. dx ;;

(* exercise 1.3 *)

let smooth f dx = fun x -> (f(x) +. f(x -. dx) +. f(x +. dx)) /. 3.0 ;;

(* exercise 1.4 *)  
  
let rec power1 a n =
  if n = 0 then 1.0 else a *. power1 a (n - 1) ;;

let sq x = x *. x ;;
  
let rec power2 a n =
  if n = 0 then 1.0 else
    if n mod 2 = 0 then sq ( power2 a (n / 2)) else
      a *. sq ( power2 a (n / 2 )) ;;

#trace power1 ;;
  
#trace power2 ;;
  
power1 5.0 13 ;;
  
power2 5.0 13 ;;

(* exercise 1.5 *)  

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b) ;;

(* exercise 1.6 *)

let rec inner_is_prime n i =
  if i >= 1 + int_of_float (sqrt (float_of_int n)) then true
  else if n mod i = 0 then false
  else inner_is_prime n (i+1) ;;
  
let rec is_prime n =
  if n = 2 then true
  else inner_is_prime n 2 ;;
  
let rec biggest_prime n =
  if is_prime n then n
  else biggest_prime (n-1) ;;

(* long computation...  
 * biggest_prime max_int ;;
 * result: int = 4611686018427387847
 *)

(* exercise 1.7 *)

  let _or_ a b c =
    match (a,b,c) with
      (true,_,_) -> true
    | (_,true,_) -> true
    | (_,_,c) -> c ;;

(* exercise 1.8 *)
(* pen and paper, can be checked in the repl *)
