(* 2M1 *)

(* a *)
infix 6 ++
fun (a, b) ++ (c, d) = (a + c, b + d);
val infix_add = (3, 5) ++ (5, 2) = (8, 7);

infix 7 **
fun (a, b) ** (c, d) = (a * c - b * d, b * c + a * d);
val infix_multi = (2, 3) ** (4, 3) = (~1, 18);

(* b *)
infix 6 ~~
fun (a, b) ~~ (c, d) = (~a, ~b) ++ (~c, ~d);
val infix_minus = (3, 4) ~~ (5, 1) = (~8, ~5);

infix 7 //
fun (1 // (a, b)) = (a div (a * a + b * b), ~b div (a * a + b * b));

(*
fun testdiv (a, b) = abs(1 // (a, b)) < 0.000000001;
val infix_div = (1 // (2, 3)) = testdiv((0.166666, ~0.25));
*)

(* 2M2 *)
(*
fun visklokken (kl : int * int)
    = Int.toString t ^ "timer og " ^ Int
*)

(* 4.1 IP-2 *)
fun validTime(h, m ,s) = h >= 0 andalso h <= 24 andalso
                         m >= 0 andalso m <= 60 andalso
                         s >= 0 andalso s <= 60;

(* 4.4 IP-2 *)
fun nullC (n) = if n < 10
                then n
                else n mod 10 * nullC (n div 10);

(* 2M3 *)
fun gcd(m) = if #1 m = 0 then #2 m
             else gcd(#2 m mod #1 m, #1 m);
