(**********)
(* Mandag *)
(**********)

(* 4M1 *)

val aString = ["a", "b", "c", "d"];

(*
 * fun parantes []      = raise Empty
 *   | parantes (xs) = map (fn(n) => "(" ^ n ^ ")" ) xs;
 *)
fn x => "(" ^ x ^ ")";

map (fn x => "(" ^ x ^ ")") ["a", "b", "c", "d"];

(* 4M2 *)

val talString = [1,2 ,3 ,4 ,5 ];

fun differences [] = []
  | differences (x::xs) = map (fn (x, y) => y-x) (ListPair.zip(x::xs, xs));

(* 4M3 *)

fun lookup [] key = NONE
  | lookup ((a, b):: xs) key = if key = a then SOME b
                               else lookup xs key;

(* 4M4 *)

fun smallest [] = NONE
  | smallest [x] = SOME x
  | smallest (x::x2::xs) = if x < x2
                           then smallest (x::xs)
                           else smallest (x2::xs);

(* 4M5 *)

fun somes xs = foldr (fn (x, acc) => case x of
                                       NONE   => acc
                                     | SOME k => k :: acc) [] xs;


(* 4M6 *)

local
  fun gcd (0, n) = n
    | gcd (m, n) = gcd(n mod m, m)
in
  fun gcdListe xs = foldl gcd 0 xs
end;

(* 4M7 *)

fun uncurry f (x, y) = f x y;

fun curry f x y = f (x, y);


(***********)
(* Tirsdag *)
(***********)

(* 4T1 *)

datatype solution = Zero | One of real | Two of real * real;

fun solve2 (a, b, c) = let
                         val d = b*b - 4.0 * a * c
                       in
                         case (Real.compare(d, 0.0)) of
                            LESS    => Zero
                          | EQUAL   => One (~b / (2.0 * a))
                          | GREATER => Two ((~b - Math.sqrt(d)) / (2.0 * a),
                                            (~b + Math.sqrt(d)) / (2.0 * a))
                       end;

val test_4I1_0 = solve2(2.0, 3.0, 1.0) = Two(~1.0, ~0.5);
val test_4I1_1 = solve2(2.0, 3.0, 4.0) = Zero;
val test_4I1_2 = solve2(1.0, 4.0, 4.0) = One(~2.0);

(* 4T2 *)

datatype point = point of real * real
datatype shape = Circle of point * real
               | Square of point * real
               | Triangle of point * real * real * real;
(*
fun translate (x ,y) sh = case sh of
                              Circle (Point(x1, y1), r) =>
                                  Circle(Point(x1 + x, y1 + y), r)
                            | Square (Point(x1, y1), r) =>
                                  Square (Point(x1 + x, y1 + y), r)
                            | Triangle (Point(x1, y1), r1, r2, r3) =>
                                  Triangle (Point(x1 + x, y1 + y), r1, r2, r3);

val test_4I2_0 = translate (~2.0, 3.0);
                 (Circle
*)

(* 4T3 *) (* brøkker *)

datatype rational = Ratio of int * int;


local
  fun gcd (0, n) = n
    | gcd (m, n) = gcd(n mod m, m)
  fun lcm (n, m) = (n * m) div gcd(n, m)
in
  fun rationalCompare (Ratio(x, y), Ratio(x2, y2)) = let
                                                       val l = lcm(y, y2)
                                                     in
                                                       Int.compare(x*1 div y,
                                                                   x2*1 div y2)
                                                     end
end;

(* 4T4 *)

load "Listsort";

fun sortRatList xs = Listsort.sort rationalCompare xs;

sortRatList [Ratio(1,3),Ratio(7,8),Ratio(5,6),Ratio(2,3)];

val test_4T4_0 = sortRatList [Ratio(1,3),Ratio(7,8),Ratio(5,6),Ratio(2,3)] =
                 sortRatList [Ratio(1,3),Ratio(2,3),Ratio(5,6),Ratio(7,8)];

(**********)
(* Fredag *)
(**********)

(* 4F1 *)
(* kopi fra uge seddel*)(*
fun partition (_ , []) = ([], [])
  | partition (pivot:real, x::xr) = let
    val (xv, xh) = partition (pivot, xr)
      in
        if   x <= pivot
        then (x::xv, xh)
        else (xv, x::xh)
      end

fun quicksort [] = []
  | quicksort (x :: xr)
    = let val (xv, xh) = partition (x, xr)
      in quicksort xv @ x :: quicksort xh
      end;
*)

fun part _ _ [] = ([],[])
  | part f pivot (x::xs) = let val (xv, xh) = part f pivot xs
                           in
                             if f(x, pivot) = LESS orelse f(x, pivot) = EQUAL
                             then (x::xv, xh)
                             else (xv, x::xh)
                           end;

fun qsort f [] = []
  | qsort f (x::xs) = let val (xv, xh) = part f x xs
                      in qsort f xv @ x :: (qsort f xh)
                      end;

val test_4F1 = qsort Int.compare [1,7,2,6,3,4] = [1,2,3,4,6,7];

(* 4F2 *)

fun splitWhen _ [] = ([], [])
  | splitWhen f (xs) = foldr (fn (e, (ys, zs)) => if f(e)
                                                  then (ys, e::zs)
                                                  else (e::ys, zs))
                                  ([], []) xs;

val test_4F2 = splitWhen (fn x => x > 2) [1,2,6,3,1,6,4];

(* 4F3 *)

fun helper _ (xs, []) = (xs, [])
  | helper f (xs, y::ys) = if f(y) then (xs, y::ys)
                           else helper f (xs @ [y], ys);

fun splitwhen2 f (xs) = helper f ([], xs);
(*
fun fields s = let val (xv, x::xh) = splitWhen2 (fn x => x = ",") (explode(s));
               in
                 implode(xv) :: [implode(xh)]
               end
*)
local 
fun helper xs (s::ss) = let val (xv, xh) = if s = #","
                                           then splitWhen2 (fn x => x = #",")(ss)
                                           else splitWhen2 (fn x => x = #",")(s::ss)
                        in
                          helper (xs @ [implode(xv)]) xh
                        end
in
fun fields s = let val (x::xs) = explode s
               in if x = "," then "" :: (helper [] xs)
                  else helper [] (x::xs)
               end
end;
