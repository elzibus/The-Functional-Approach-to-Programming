
(* the authors solutions can be found here for all but exercise 4.6 :
   http://pauillac.inria.fr/cousineau-mauny/sols-chap4.txt

   They are reproduced below and modified to work in Ocaml.
 *)

(* -- Ex 4.1 -------------------------------------------------------------------- *)

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;

(* Using the option type to locally encode success and failure  *)

let search p t =
  let rec srch p t = match t with
      Leaf elt -> if p(elt) then Some elt else None
    | Node(t1,t2) ->
	match srch p t1 with
	    None -> srch p t2
	  | res -> res
  in match srch p t with
      Some e -> e
    | None -> failwith "Not found"
;;

(* Using an extra argument in the local function. This extra argument
   contains the trees not yet traversed. *)

let search p t =
  let rec srch p t ts = match t with
      Leaf elt ->
        if p(elt) then elt else 
          (match ts with
               [] -> raise (Failure "Not found")
             | t1 :: ts -> srch p t1 ts)
    | Node(t1,t2) -> srch p t1 (t2::ts)
  in srch p t []
;;

(* Now, the exception Found defined below represents success, and holds the
   element we are searching for. *)

(* ! exceptions cannot be polymorphic in Ocaml *)
  
exception Found of int ;;

let search p t =
  let rec srch p t = match t with 
     Leaf elt -> if p(elt) then raise (Found elt) else ()
    | Node(t1,t2) -> srch p t1; srch p t2 in
  try srch p t; failwith "Not found"
  with Found e -> e
;;

search (fun x -> x = 1) (Node(Leaf 7, Node( Leaf 1, Leaf 3))) ;;

search (fun x -> x = 2) (Node(Leaf 7, Node( Leaf 1, Leaf 3))) ;;

  
(* -- Ex 4.2 -------------------------------------------------------------------- *)

(* Borrowed and edited from exercise 1.1: *)

let solutions a b c =
  print_string "The equation: ";
  print_float a;
  print_string " x^2 + ";
  print_float b;
  print_string " x + ";
  print_float c;
  print_string " = 0 has ";
  let delta = (b *. b) -. (4.0 *. a *.c) in
  if delta < 0.0 then print_string "no solution" else
  if delta > 0.0 then
    let s = sqrt(delta) in
      print_string "two solutions:\n";
      print_string "   x = "; print_float ((-. b -. s) /. (2.0 *. a));
      print_string "\n   x = "; print_float ((-. b +. s) /. (2.0 *. a))
  else
    (print_string "only one solution:\n";
     print_string "   x = ";
     print_float (-.b /. (2. *. a)));
  print_newline()
;;

(* -- Ex 4.3 -------------------------------------------------------------------- *)

let gensym =
  let count = ref (-1) in
  fun str -> count := !count  + 1;
    str ^ (string_of_int !count)
;;

(* -- Ex 4.4 -------------------------------------------------------------------- *)


(* We need to define both gensym and reset_gensym in such a way that they
   internally share the count reference *)

let (gensym, reset_gensym) =
  let count = ref (-1) in
  ( (fun str ->
       count := !count  + 1;
       str ^ (string_of_int !count)),
    (fun () -> count := -1)
  )
;;

gensym "" ;;  

gensym "" ;;  

reset_gensym() ;;  

gensym "" ;;  

(*  -- Ex 4.5 -------------------------------------------------------------------- *)

type 'a priority = ('a * int) list;;

let rec insert elt prio plist = match plist with
    [] -> [(elt, prio)]
  | ((x,p) as xp)::xps ->
      if prio > p then (elt, prio)::plist
      else xp :: (insert elt prio xps)
;;

(* -- Ex 4.6 -------------------------------------------------------------------- *)

(* TODO *)

(* -- Ex 4.7 -------------------------------------------------------------------- *)

type 'a node = { mutable info: 'a; mutable succs: ('a node) list};;

(* -- Ex 4.8 -------------------------------------------------------------------- *)

type cell = {mutable car: sexpr; mutable cdr: sexpr}
and sexpr =
    Symbol of string
  | Cell of cell
;;

let rplaca c x = c.car <- x
and rplacd c x = c.cdr <- x
;;

(* -- Ex 4.9 -------------------------------------------------------------------- *)

(*  
In Pascal, when a procedure/function modifies an argument passed by
reference, the modification persists after the procedure/function
returns. In Caml, this behavior may be simulated by passing a
reference to a value instead of passing a value. (Note that in C, one
would pass a pointer to a value to obtain the same behavior.)

For instance, a function incrementing a reference to an integer could
be written as:

let incr r = r := !r + 1;;

If r0 is a reference to 0, after passing r0 to incr, r0 would be a
reference to 1:
 *)
let r0 = ref 0;;

incr r0;;

r0;;

(* => ref 1 *)
