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
  Create a list for powers
*)
let rec to_pow (pL: pExp list) (e: Expr.expr) (i: int): pExp =
  if i <> 0 then
    to_pow (List.cons (from_expr e) pL) e (i - 1)
  else
    Times(pL)
and
(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
from_expr (e: Expr.expr) : pExp =
  match e with 
    | Num(i) -> Term(i, 0)
    | Var(_c) -> Term(1, 1)
    | Add(e1, e2) -> Plus([(from_expr e1); (from_expr e2)])
    | Sub(e1, e2) -> Plus([(from_expr e1); Times([Term(-1, 0); (from_expr e2)])])
    | Mul(e1, e2) -> Times([(from_expr e1); (from_expr e2)])
    | Pow(e, i) -> to_pow [] e i
    | Pos(e) -> from_expr e
    | Neg(e) -> Times([Term(-1, 0); (from_expr e)])

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let rec degree (e: pExp): int = 
  match e with 
    | Term(n,m) -> m
    | Plus(eL) -> List.fold_left (fun maxNum item -> max maxNum (degree item)) 0 eL
    | Times(eL) -> List.fold_left (fun sumNum item -> sumNum + (degree item)) 0 eL

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare_degree (e1: pExp) (e2: pExp) : int =
  compare (degree e2) (degree e1)

let rec print_pExp_r (e: pExp): unit = 
  match e with
    | Term(i1, i2) -> 
      begin
        match (i1, i2) with 
          | (0, _) -> Printf.printf "0"
          | (i1, 0) -> Printf.printf "%d" i1
          | (1, 1) -> Printf.printf "x"
          | (i1, 1) -> Printf.printf "%dx" i1
          | (1, i2) -> Printf.printf "x^%d" i2
          | (i1, i2) -> Printf.printf "%dx^%d" i1 i2
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
        Printf.printf "(";
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
  print_pExp_r e;
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

(* Create a list of random ints with range [0-99) *)
let rec rand_list (iL: int list) (size: int): int list =
  Random.self_init ();
  if size <> (List.length iL) - 1 then
    (* TODO: Revise this to work with all of the tests *)
    rand_list ((Random.int 99)::iL) size
  else 
    iL

(* Compute if two pExp are equivalent *)
let equal_pExp (e1: pExp) (e2: pExp): bool =
  (* Get the power for the n+1 calculation *)
  let power = degree e1 in
  let values = rand_list [] power in
  (* Printf.printf "Power is %d\n" power; *)
  List.fold_left (fun b x -> 
    (* Printf.printf " Value is %d, original is %d, new is %d\n" x (eval e1 x) (eval e2 x); *)
    if b && (eval e1 x) = (eval e2 x) then true else false
  ) true values

(* Check the structure of two pExp and see if they match exactly *)
let rec hit_fix_point (lastRun: pExp) (thisRun: pExp) (result: bool): bool =
  (* Quick out if one thing doesn't match *)
  if result = false then
    result
  else
    match (lastRun, thisRun) with
    | (Term(n1, m1), Term(n2, m2)) -> (
      if n1 = n2 && m1 = m2 then
        true
      else
        false
    )
    | (Plus(l1), Plus(l2)) -> 
      if (List.length l1) = (List.length l2) then
        List.fold_left2 (fun res i1 i2 -> hit_fix_point i1 i2 res) result l1 l2
      else 
        false
    | (Times(l1), Times(l2)) -> 
      if (List.length l1) = (List.length l2) then
        List.fold_left2 (fun res i1 i2 -> hit_fix_point i1 i2 res) result l1 l2
      else 
        false
    | _ -> false
    
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
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Plus[Term(3,3); Term(4,4)]] 
      => Plus[Times[Term(1,1); Plus[Term(3,3); Term(4,4)]]; Times[Term(2,2); Plus[Term(3,3); Term(4,4)]]
      => 
  Hint 6: Find other situations that can arise
*)

let rec simplify1 (e: pExp): pExp =
    (* Printf.printf "Hit simplify1\n"; *)
    match e with
      (* Return basic terms *)
      | Term(n, m) -> e
      | Plus(l) ->
        begin
          let rec add_list (head: pExp list) (tail: pExp list): pExp list =
            (* Printf.printf "Hit add\n"; *)
            match tail with 
              (* Regular math *)
              | Term(n1, m1)::Term(n2, m2)::tl ->
                if m1 = m2 || n2 = 0 then
                  add_list head (Term(n1 + n2, m1)::tl)
                else
                  add_list (head@[Term(n1, m1)]) (Term(n2, m2)::tl)
              (* Flatten out *)
              | Plus(l)::tl -> add_list head (l@tl)
              (* If don't know, simplify and move past *)
              | e::tl -> add_list (head@[simplify1 e]) tl
              (* Once done iterating over everything, return the new list *)
              | [] -> head 
          in
          (* Go through list and simplify then sort *)
          let l = add_list [] l in
          let l = List.sort compare_degree l in
          match l with
            (* If it only holds one item, pull it out *)
            | hd::[] -> hd
            | _ -> Plus(l)
        end
      | Times(l) ->
        begin
          let rec multiply_list (head: pExp list) (tail: pExp list): pExp list =
            (* Printf.printf "Hit multiply\n"; *)
            match tail with 
              (* Regular math *)
              | Term(n1, m1)::Term(n2, m2)::tl -> multiply_list head (Term(n1 * n2, m1 + m2)::tl)
              (* Flatten out *)
              | Times(l)::tl -> multiply_list head (l@tl)
              (* Term * Plus *)
              | Term(n1, m1)::Plus(l)::tl | Plus(l)::Term(n1, m1)::tl -> 
                let t = List.map (fun i -> 
                  match i with
                    | Term(n2, m2) -> Term(n1 * n2, m1 + m2)
                    | _ -> Times(multiply_list [] [Term(n1, m1); i])
                ) l in
                multiply_list head ((Plus(t))::tl)
              (* Distribution *)
              | Plus(l1)::Plus(l2)::tl -> 
                  multiply_list (head@(
                  let l = List.map (fun i -> Times(multiply_list [] [i; Plus(l2)])) l1 in
                  [Plus(l)]
                )) tl
              (* If don't know, simplify and move past *)
              | e::tl -> multiply_list (head@[simplify1 e]) tl
              (* Once done iterating over everything, return the new list *)
              | [] -> head
          in
          (* Go through list and simplify *)
          let l = multiply_list [] l in
          match l with
            (* If it only holds one item, pull it out *)
            | hd::[] -> hd
            | _ -> Times(l)
        end

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e: pExp): pExp =
  let rE = simplify1 e in
  if hit_fix_point e rE true then
    e
  else (
    print_pExp rE;
    simplify rE
  )
  
(* Call simplify and check if the final value is the same as the first *)
let check_simplify (e: pExp): pExp =
  print_pExp e;
  let final = simplify e in
  if (equal_pExp e final) then
    Printf.printf "The values are the same\n"
  else  
    Printf.printf "The values are not the same\n"
  ;
  final