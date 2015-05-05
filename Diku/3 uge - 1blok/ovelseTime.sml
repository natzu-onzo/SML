(* David Jørgensen *)

(* mandag *)

(* 3M1 *)

val tal = [1, 2, 3, ~3, 5, ~2, 0, ~4, ~5, 2, ~4];

local
fun isOkay x = x > 0;
in
fun sumIfOkay [] = 0
  | sumIfOkay (x::ys) = if isOkay(x)
                         then x + sumIfOkay(ys)
                         else sumIfOkay(ys);
end

(* 3M2 *)

val tal2 = [[1,2],[5,4,3]];

fun revrev [] = []
  | revrev (x::xs) = revrev(xs) @ [rev(x)];

(* 3M3 *)

;use "InstagraML.sml";
;use "Effects.sml";
open InstagraML;
(* open Effects; *)
val torben = InstagraML.readBMP ("torben.bmp");

fun counterClockwise img = clockwise(clockwise(clockwise(img)));

val test_CCW_torben = writeBMP("counterToben.bmp",counterClockwise(torben));

(* 3M4 *)

fun above (img1, img2) = if width(img1) = width(img2)
                         then counterClockwise(beside(clockwise(img1),
                                                      clockwise(img2)))
                         else raise Domain;

val double_torben = writeBMP("doubleTorben.bmp", above(torben, torben));

(* 3M5 *)

fun imageConcat [] = raise Empty
  | imageConcat [img] = img
  | imageConcat (img1 :: img2 :: imgs)  = beside(img1, imageConcat(imgs));

(*
val torbens = writeBMP("muchTorben.bmp", imageConcat([torben,torben,torben,torben])); *)

(* 3M6 *)

val counterClockwise = clockwise o clockwise o clockwise;

(***********)
(* Tirsdag *)
(***********)

(* 3T1 *)

fun colourAverage (r1, g1, b1) (r2, g2, b2) = ((r1 + r2) div 2,
                                               (g1 + g2) div 2,
                                               (b1 + b2) div 2);

writeBMP("mix.bmp", scale 100.0 100.0 (pixel (colourAverage
                                                  (200, 0, 0)
                                                  (0, 0, 200))));

(* 3T2 *)

fun fadeRed img = recolour (fn c => colourAverage (255, 0, 0) c) img;

writeBMP("redTorben.bmp", fadeRed torben);

(* 3T3 *)

fun roedere img = recolour (colourAverage (255, 0, 0)) img;

(* 3T4 *)

fun iterate effect img 0 = img
  | iterate effect img n = beside(img,
                                  iterate effect (effect img) (n-1));

writeBMP("redTorben.bmp", iterate roedere torben 5);

(* 3T5 *)

fun isOkay x = x > 0;
(*
fun sumIfOKay2 f [] = 0
  | sumIfOKay2 f (x::xs) = if f x
                           then x + sumIfOkay2 f xs
                           else sumIfOkay2 f xs; *)

(* 3T6 *)
(*
fun imageConcat []= raise Empty
  | imageConcat [img] = img
  | imageConcat (img1 :: imgs) = beside(img, imageConcat(img));
*)
fun imageConcat2 [] = raise Empty
  | imageConcat2 (x::xs) = foldl (fn (img, acc) => beside(acc, img)) x xs;

val Torbens = writeBMP("muchTorben2.bmp", imageConcat2 ([torben,torben,torben,torben,torben]));

(* 3T7 *)

