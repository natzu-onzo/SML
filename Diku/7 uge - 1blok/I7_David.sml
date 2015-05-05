(* David HOlberg Jørgensen *)
(* "I7_David.sml" *)

(*******)
(* 7I1 *)
(*******)

signature MSET =
sig
  type 'a mset  (* typen af multimængder med elementer af type 'a *)
  val multiplicity : ''a mset * ''a -> int (* antal forekomster af element *)
  val empty : 'a mset  (* den tomme multimængde *)
  val singleton : ''a -> ''a mset  (* laver multimængde med et element *)
  val union : ''a mset * ''a mset -> ''a mset (* foreningsmængde *)
  val intersect : ''a mset * ''a mset -> ''a mset (* fællesmængde *)
  val minus : ''a mset * ''a mset -> ''a mset (* mængdedifferens *)
end;

(*******)
(* 7I2 *)
(*******)

structure MsetF :> MSET =
struct
type 'a mset = 'a -> int

Infix member
 fun x member [] = false
   | x member(y::ys) = x=y orelse x member ys

 fun plus [] x z = z
   | plus (y::ys) x z  = if x = y then plus ys x (z + 1)
                         else plus ys x z

 fun part (pivot:real, x::xs) = let val (xv, xh) = part(pivot, xs)
                                in if x <= pivot then (x :: xv, xh)
                                   else (xv, x :: xh)
(*
 fun qsort [] = []
   | qsort (x::xs) = let val (xv, xh) = part (x, xs)
                     in qsort xv @ x :: qsort xh
                     end *)

 fun sub (x::xs) ys zs = if x member ys then sub xs ys x :: zs
                         else false 

 fun multiplicity (ys, x) = (plus ys x 0)
 fun empty []  = raise Empty
   | empty [x] = raise Domain

 fun singleton x = (fn y => if y = x then 1 else 0)
 fun intersect (xs, ys) = let val = sub xs ys []

 fun union (m1, m2) = qsort(fn x => m1 x + m2 x)

end;
