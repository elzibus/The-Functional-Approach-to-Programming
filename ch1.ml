
(*
 * Hints for the Ocaml user, this was run on Ocaml 4.03.0
 * This should not be used to learn ocaml syntax, it's mostly about making the code in the book run in Ocaml.
 *)

(* TYPO: first page of the book, missing "be" in "They show how the ideas can >< implemented in the ..." *)

(* 1.1 Expressions and Definitions *)

(* 1.1.1 Expressions *)

2 + 3 ;;

1 + 2 * 3 ;;

(1 + 2) * 3 ;;    

1 - 2 + 3 ;;

1 - (2 + 3) ;;

4 / 2 * 2 ;;

4 / (2 * 2) ;;

(* 1.1.2 Definitions *)  
  
let x = 2 + 3 ;;

let pi = 3.14 ;;

x * x ;;

(* 1.1.3 Local Definitions *)  
  
let x = 2 in x + 1 ;;

let a = 3 and b = 4 in a * a + b * b ;;

x ;;

(*
 * "where" for local definitions is not in ocaml anymore apparently

   x + 1 where x = 2 ;;                     <- fails
   a * a + b * b where a = 3 and b = 4 ;;   <- fails
 *)

(* 1.2 Elementary Types *)

(* 1.2.1 Integers *)

(* 1.2.2 Floating-Point Numbers *)  
  
1.0 ;;

(* this generates and error because sqrt expects a float:
 *        sqrt (2) ;;
 *)

sqrt (2.) ;;

sqrt(float_of_int(2)) ;;

acos( -1.) ;;

(* re-written with let because of absence of "where" in ocaml *)
  
let x = sin(1.0) in let y = cos(1.0) in
    x *. x +. y *. y ;;

(* 1.2.3 Floating Mode *)
  
(* the 'open "float" ;;' as described in the book does not work with a simple
 * invokation of ocaml on the command line ( ie, plain "ocaml" ).
 * I don't know if this is still present in the current Ocaml.
 *)

(* 1.2.4 Booleans *)
  
1 < 2 ;;

  (* use && instead of &
     use || instead of or
   *)
  
1 <= 2 && (0 < 1  || 1 < 0) && not(2 < 2) ;;

let x = 3 in
    if x < 0 then x else x * x ;;

(* 1.2.5 Character Strings *)

"a string" ^" of characters" ;;

(* 1.2.6 Characters *)
(* ! characters appear between single quotes in Ocaml *)
  
'a' ;;

int_of_char 'a' ;;  

(* 1.3 Cartesian Product *)

(1, 1.2) ;;  

(1 + 2, true || false, "hello") ;;

((1,2), (3,4)) ;;

snd(1 + 2,3 + 4);;

fst((1,2),3) ;;

let p=(1,2) in
    (snd(p), fst(p)) ;;

(* 1.4 Functions *)  
  
(* 1.4.1 Defining Simple Functions *)

let sq(x) = x *. x ;;

sq(2.0) ;;

(* "module" has a meaning in ocaml and cannot be used as a function name *)

let module_(x,y) = sqrt( sq(x) +. sq(y)) ;;

module_(3., 4.) ;;

(* 1.4.2 Function Expressions *)

fun(x,y) -> sqrt(  sq(x) +. sq(y)) ;;

(fun(x,y) -> sqrt(  sq(x) +. sq(y)))(3., 4.) ;;

let sq = (fun x -> x *. x);;
  
(* 1.4.3 Higher Order Function Expressions *)

let h = (fun f -> (fun x -> f(x) /. x)) ;;

(h(sin))(0.1) ;;

let k = h(sin) ;;

k(0.1) ;;  

k(0.0000001) ;;

let h = fun f x -> f(x) /. x ;;  
  
(* 1.4.4 Recursive Functions *)  

let x = 1 ;;

let x = x + 2 ;;
  
let rec fact n =
  if n = 0 then 1 else n * fact (n - 1) ;;

(* 1.4.5 Mutually Recursive Functions *)

let rec even n = if n = 0 then true else odd(n-1)
and odd n = if n = 0 then false else even (n-1) ;;

(* 1.4.6 Functions Defined by Case *)

(* ! ocaml uses function instead of fun *)
  
let neg = function
    true -> false
  | false -> true ;;
  
let xor = function
    (false, false) -> false
  | (true, true) -> false
  | (false, true) -> true
  | (true, false) -> true ;;
  
let xor = function
    (false, x) -> x
  | (true, x) -> neg x ;;
  
let rec fact = function
    0 -> 1
  | n -> n * fact (n - 1) ;;
  
let rec fib = function
    0 -> 1
  | 1 -> 1
  | n -> fib(n-1) + fib(n-2) ;;
  
let conj = function
    (true, true) -> true
  | (_,_) -> false ;;
  
let conj = function
    (true, true) -> true
  | _ -> false ;;
  
(* 1.4.7 The function Construction *)

(* from the Ocaml FAQ:
   Functions that use pattern-matching are introduced by the keyword function.
 *)

let conj = function
    (true, true) -> true
  | _ -> false ;;

(* 1.4.8 The match with Construction *)

  match (3,5) with
    (0, _) -> 0
  | (x,y) -> x*y ;;

(* 1.5 Polymorphism *)
    
(* 1.5.1 Type Variables *)

fst ;;
  
let fst(x,y) = x ;;

let snd(x,y) = y ;;

let proj_23 (x,y,z) = y ;;

let id = fun x -> x ;;
  
id(3) ;;

(id(id)) (id(3), id(4)) ;;  

let compose (f,g) = fun x -> f(g(x)) ;;

let square(x) = x * x in
    (compose(square, square))(3) ;;

fun ((x:int),(y:bool)) -> (y,x) ;;

fun(x,y) -> ((y:bool),(x:int)) ;;  

fun((x,y):int*bool) -> (y,x) ;;

((fun (x,y) -> (y,x):int*bool -> bool*int)) ;;

fun ((x:int),y) -> (y,x) ;;  

(* 1.5.2 Type Synthesis *)

(* 1.6 About Syntax *)

(* 1.6.1 Application Notation *)

(* 1.6.2 Syntactic Analysis of Caml Expressions *)

let d x = (x, x) ;;  

let s x = x + 1 ;;

d 2,3 ;;

(* ! 1,2 is really (1,2) *)

d (2,3) ;;  

s 2*3 ;;

s (2*3) ;;

(*1.6.3 Infix Operators Dfined by the User *)

(* to use operators in prefix notation use them enclosed in parens *)
  
( + ) ;;

(* rules for infix operators definitions are different in Ocaml *)  

let ( --- ) f g = fun x -> f(g(x)) ;;

(sq --- sq) 2.0 ;;
  
(id --- id) 1 ;;
  
(id --- id) id ;;

(* 1.7 Scope of Identifiers *)
  
let x = 7 in
    let f z = z*x in
    let x = true in f 3 ;;

(* Ocaml produces a warning "unused variable x" *)

(* 1.8  More about Function Types: Equivalent Function Spaces *)

let f1 = fun (x,y) -> 2*x + 3*y ;;  
  
let f2 = fun (y,x) -> 2*x + 3*y ;;  

let f3 = fun x y -> 2*x + 3*y ;;

let f4 = fun y x -> 2*x + 3*y ;;

(* TYPO: isomorhisms *)  

let curry f x y = f (x,y) ;;

let uncurry f (x,y) = f x y ;;

let perm f (x,y) = f(y,x) ;;

let perm' f x y = f y x ;;
  
(* 1.8.1 The Types of Predefined Operators *)

(+) ;;

(* not the space to avoid starting a comment *)
( *. );;  

(* add_int and mul_float are not in Ocaml *)

(* 1.9 Examples: Expressive Power *)

(* 1.9.1 Bounded Iterations *)

let rec iter n f = if n = 0 then id else compose(f, iter (n-1) f) ;;

iter 4 (fun x -> x*x) 2 ;;
  
let rec iter n f x = if n=0 then x else f(iter (n-1) f x) ;;

let fib n = fst(iter n (fun (x,y) ->(x+y,x)) (1,0)) ;;

(* BUG: The value of fib 50 in the book is wrong *)

(* 1.9.2 Unbounded Iterations *)

let rec loop p f x = if (p x) then x else loop p f (f x) ;;  

let iter n f x = snd(loop (fun (p,x) -> n=p)
			  (fun (p,x) -> (p+1,f x))
			  (0, x)) ;;
  
(* 1.9.3 Binary Method *)

(* no <. in Ocaml *)
  
let dicho (f,a,b,epsilon) =
  let is_ok(a,b) = abs_float(b-.a) < epsilon in
  let do_better(a,b) =
    let c=(a+.b)/.2.0 in
    if f(a) *. f(c) > 0.0 then (c,b)
    else (a,c) in
  loop is_ok do_better (a,b) ;;
  
dicho ((fun x -> cos(x /. 2.0)), 3.1, 3.2, 1e-10);;

(* 1.9.4 Newton's method *)

let deriv (f, dx) x =(f(x+.dx)-.f(x))/.dx ;;

let newton (f, start, dx, epsilon) =
  let is_ok x = abs_float(f x) < epsilon in
  let do_better x =
    let f' = deriv (f, dx) in
    (x -. f x/. f' x) in
  loop is_ok do_better start ;;
  
newton (cos, 1.5, 1e-10, 1e-10) *. 2.0;;

newton((fun x -> log x -. 1.0), 2.7, 1e-10, 1e-10) ;;
  
(* 1.9.5 About Iteration *)

(* 1.9.6 Summations *)

let rec sigma f (a,b) =
  if a > b then 0
  else (f a) + sigma f (a+1, b);;

let rec summation (incr, test) (op, e) f a =
  if test a then e
  else op (f a) (summation (incr, test) (op,e) f (incr a)) ;;

let sum (op,e) f a b dx =
  summation ((fun x -> x +. dx), (fun x -> x > b)) (op,e) f a ;;

let integrate f a b dx =
  sum(( +. ), 0.0) (fun x -> f(x) *. dx) a b dx ;;

integrate (fun x -> 1.0/.x) 1.0 2.0 0.001;;  

let summation_int (op,e) f a b =
  summation ((fun x -> x+1), (fun x -> x>b)) (op,e) f a;;

let sigma = summation_int (( +. ), 0.0) ;;  

(* ! this is not pi the number *)
  
let pi = summation_int (( *. ), 1.0) ;;  

let fact = pi float_of_int 1 ;;  

fact 10 ;;  

sigma (fun n -> 1.0/.fact n) 0 20 ;;  

(* 1.10 Summary *)

(* 1.11 To Learn More *)

  
