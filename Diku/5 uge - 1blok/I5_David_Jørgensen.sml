(* David Jørgensen *)

;use "InstagraML.sml";

datatype primaryColours = Black | Red | Green | Blue
                          | White | Cyan | Magenta | Yellow

type point = real * real (* et punkt i planen *)

datatype 'col figure =
         Circle of 'col * point * real (* farve , center og radius *)
         | Rectangle of 'col * point * point
                        (* farve , nederste venstre og oeverste hoejre hjoerne *)
         | Over of 'col figure * 'col figure;
                        (* foerste figur over anden figur *)

val redCircleOverBlueSquare = Over (Circle (Red, (0.0 , 0.0), 1.8),
                                   Rectangle (Blue, (~1.5 , ~1.5), (1.5 , 1.5)));

val rectangle1 = Rectangle (Black, (~1.5 , ~1.5), (1.5 , 1.5));
val circle1 = Circle (Green, (0.0 , 0.0), 1.8);

(*******)
(* 5I1 *)
(*******)

(* change position by putting last one, to be the first one *)
fun reorder (Over(x, z)) = (Over(z, x))
  | reorder (Circle (color, (x, y), z)) = (Circle (color, (x, y), z))
  | reorder (Rectangle (color, (x, y), z)) = (Rectangle (color, (x, y), z));

val Test_5T1_1 = reorder redCircleOverBlueSquare =
                 Over(Rectangle(Blue, (~1.5, ~1.5), (1.5, 1.5)),
                      Circle(Red, (0.0, 0.0), 1.8));
val Test_5T1_2 = reorder circle1 = Circle(Green, (0.0, 0.0), 1.8);
val Test_5T1_3 = reorder rectangle1 = Rectangle(Black, (~1.5, ~1.5), (1.5, 1.5));

(*******)
(* 5I2 *)
(*******)

(* scaling one or more figures up *)
fun scale (Over(a, b)) q = Over(scale a q, scale b q)
  | scale (Circle (color, (x, y), z)) q =
                               Circle (color, (x * q, y * q), (z * q))
  | scale (Rectangle (color, (x, y), (z, a))) q =
                               Rectangle (color, (x * q, y * q), (z * q, a * q));


val Test_5I2_1 = scale redCircleOverBlueSquare 2.0 =
                 Over(Circle(Red, (0.0, 0.0), 3.6),
                      Rectangle(Blue, (~3.0, ~3.0), (3.0, 3.0)))
val Test_5I2_2 = scale circle1 2.0 = Circle(Green, (0.0, 0.0), 3.6);
val Test_5I2_3 = scale rectangle1 2.0 = Rectangle(Black, (~3.0, ~3.0), (3.0, 3.0));

(*******)
(* 5I3 *)
(*******)

(* moving the figure by "a" and "b" *)
fun move (Over(x, y)) (a, b) = Over(move x (a, b), move y (a, b))
 | move (Rectangle (color, (x, y), z)) (a, b) =
        (Rectangle(color, (x + a, y + b), z))
 | move (Circle (color, (x2, y2), z)) (a, b) =
        Circle(color, (x2 + a, y2 + b), z);

val Test_5I3_1 = move redCircleOverBlueSquare (2.0, 2.0) =
                 Over(Circle(Red, (2.0, 2.0), 1.8),
                      Rectangle(Blue, (0.5, 0.5), (1.5, 1.5)));
val Test_5I3_2 = move circle1 (2.0, 2.0) = Circle(Green, (2.0, 2.0), 1.8);
val Test_5I3_3 = move rectangle1 (2.0, 2.0) = Rectangle
                                                  (Black, (0.5, 0.5), (1.5, 1.5));

(*******)
(* 5I4 *)
(*******)

(* takes figures and putting them in a list *)
fun toList (Over(x, y)) = toList(x) @ toList(y)
  | toList (Circle(x, y, z)) = [Circle(x, y, z)]
  | toList (Rectangle(x, y, z)) = [Rectangle(x, y, z)];

val Test_5I4_1 = toList redCircleOverBlueSquare =
                 [Circle(Red, (0.0, 0.0), 1.8),
                  Rectangle(Blue, (~1.5, ~1.5), (1.5, 1.5))];
val Test_5I4_2 = toList circle1 = [Circle(Green, (0.0, 0.0), 1.8)];
val Test_5I4_3 = toList rectangle1 = [Rectangle(Black, (~1.5, ~1.5), (1.5, 1.5))];

(*******)
(* 5I5 *)
(*******)

(* taking figures out from a list *)
fun fromList []      = raise Empty
  | fromList [x]     = (x)
  | fromList (x::xs) = Over(x, fromList(xs));

val Test_5I5_1 = fromList(toList rectangle1) =
                 Rectangle(Black, (~1.5, ~1.5), (1.5, 1.5));
val Test_5I5_2 = fromList(toList circle1) = Circle(Green, (0.0, 0.0), 1.8);
val Test_5I5_3 = fromList(toList redCircleOverBlueSquare) =
                 Over(Circle(Red, (0.0, 0.0), 1.8),
                      Rectangle(Blue, (~1.5, ~1.5), (1.5, 1.5)));

(*******)
(* 5I6 *)
(*******)

(* virker ikke som den skal, den tag et ekstra element med, 
 * men den køre! og tag din ud fra en liste *)

(* taking figures out from a list, by using "foldl" funktion *)
fun fromList2 [] = raise Empty
  | fromList2 [x] = x
  | fromList2 (x::xs) = foldr(fn xs => Over(x, Over(xs))) x xs;

(*
val Test_5I6_1 = fromList2(toList redCircleOverBlueSquare) =
                 Over(Over(Rectangle(Blue, (~1.5, ~1.5), (1.5, 1.5)),
                           Circle(Red, (0.0, 0.0), 1.8)),
                      Circle(Red, (0.0, 0.0), 1.8)); *)
