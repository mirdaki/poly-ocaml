(* Random for generating ints and checking that expresions are equivalent *)
open Random

(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)


(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
let from_expr (_e: Expr.expr) : pExp =
    Term(0,0) (* TODO *)

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let degree (_e: pExp): int = 0 (* TODO *)

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2

let rec print_pExp_r (e: pExp): unit = 
  match e with
    | Term(i1, i2) -> 
      begin
        begin
        match i1 with 
          | 1 -> Printf.printf ""
          | _ -> Printf.printf "%d" i1
        end;
        begin
        match i2 with 
          | 0 -> Printf.printf ""
          | 1 -> Printf.printf "x"
          | _ -> Printf.printf "x^%d" i2
        end;
      end
    | Plus(eL) -> 
        Printf.printf "(";
        (* List.iter (fun i -> print_pExp_r i; Printf.printf " + ") eL; *)
        for i = 0 to ((List.length eL) - 1) do
          print_pExp_r (List.nth eL i);
          if i != ((List.length eL) - 1) then
            Printf.printf " + "
        done;
        Printf.printf ")"
    | Times(eL) -> 
        for i = 0 to ((List.length eL) - 1) do
          print_pExp_r (List.nth eL i);
          if i != ((List.length eL) - 1) then
            Printf.printf " * "
        done;
        Printf.printf ")"

(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)
let print_pExp (e: pExp): unit =
  print_pExp_r e
  print_newline()

(* A slightly janky way of doing int exponent *)
let exp (value: int) (power: int): int =
  (float_of_int value) ** (float_of_int power) |> int_of_float

(* Evaluates the equation e with the value of x *)
let rec eval (e: pExp) (x: int): int =
  match e with
    | Term(i1, i2) -> i1 * (exp x i2)
    | Plus(eL) -> List.fold_left (fun sum item -> sum + (eval item x)) 0 eL
    | Times(eL) -> List.fold_left (fun product item -> product * (eval item x)) 1 eL

(* Create a list of random ints with range [0-9999) *)
let rec rand_list (iL: int list) (size: int): int list =
  Random.self_init ();
  if size <> (List.length iL) - 1 then
    rand_list ((Random.int 9999)::iL) size
  else 
    iL

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (e1: pExp) (e2: pExp): bool =
  (* Get the power for the n+1 calculation *)
  let power = degree e1 in
  let values = rand_list [] (power + 1) in
  List.fold_left (fun b x -> if b && (eval e1 x) = (eval e2 x) then true else false) true values

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)
let simplify1 (e: pExp): pExp =
    e

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e: pExp): pExp =
  let rE = simplify1(e) in
    print_pExp rE;
    if (equal_pExp e rE) then
      e
    else  
      simplify(rE)
