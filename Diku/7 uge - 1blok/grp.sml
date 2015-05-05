(* 7G1 *)

signature MSET =
sig
   
   type 'a mset (* typen af multimængder med elementer af type 'a *)
   val multiplicity : ''a mset * ''a -> int (* antal forekomster af element *)
   val empty : 'a mset (* den tomme multimængde *)
   val singleton : ''a -> ''a mset (* laver multimængde med et element *)
   val union : ''a mset * ''a mset -> ''a mset (* foreningsmængde *)
   val intersect : ''a mset * ''a mset -> ''a mset (* fællesmængde *)
   val minus : ''a mset * ''a mset -> ''a mset (* mængdedifferens *)


end;

(* 7G2 *)
structure Mset : MSET
struct

  type mset = 'a mset
  fun multiplicityHelper [] n acc      = acc
  fun multiplicityHelper (x::xs) n acc = if x = n 
                                         then (multiplicityHelper xs n (acc + 1)) 
                                         else (multiolicityHelper xs n acc)
   
  fun part (_, []) = ([], [])
  | part (pivot : real, x :: xs) = let val (xv, xh) = part (pivot, xs)
                           in if x <= pivot then (x :: xv, xh)
                                else (xv, x::xh)
                           end

  fun qsort [] = []
  fun qsort (x::xs) = let val (xv, xh) = part (x, xs)
                      in qsort xv @ x :: (qsort xh)
                      end 
                                         
  fun multiplicity xs n = (multiplicityHelper xs n 0)  
  fun empty xs = raise Empty
  fun singleton n = [n]
  fun union (xs, ys) = (qsort (xs @ ys))
  
  fun intersectHelper' x y::ys = (x = y) orelse intersectHelper'(x ys)
  fun intersectHelper (x1::x2::xs) (ys) = if (intersectHelper' x1 ys) then [x1]::(intersectHelper' x2 ys) else (intersectHelper )
  
  local
  Infix member
  fun x member [] = false
  | x member(y::ys) = x=y orelse x member ys
  
  in 
  
  fun intersect [] ys = true
     |intersect ((x::xs), ys) = x member ys andalso intersect xs ys
  
  fun
end;
