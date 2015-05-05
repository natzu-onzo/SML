(* 2T1 *)
(* HR 5.2 *)
fun rmodd [] = []
  | rmodd [x] = []
  | rmodd (x1::x2::xs) = x2 :: rmodd(xs);

(* HR 5.3 *)
fun combine [] = []
  | combine [x] = raise Domain
  | combine (x1::x2::xs) = (x1, x2) :: combine(xs);

(* HR 5.7 *)
fun rmEven [] = []
  | rmEven (x::xs) = if x mod 2 = 0 then rmEven(xs)
                     else x :: rmEven(xs);

(* 2T2 *)
(* "a" *)

(*
   lave stor bogstav til lille, og  sammenligner hvem der kommer først
   i alphabetet
*)
fun bfor ([], _)        = true
  | bfor (_, [])        = false
  | bfor (x::xs, y::ys) = Char.toLower x <
                            Char.toLower y orelse
                            Char.toLower x =
                            Char.toLower y andalso
                            bfor(xs, ys);

fun ordbogsOrden (x, y) = bfor(explode(x), explode(y));

(* 2T3 *)
