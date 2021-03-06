(* David Holberg Jørgensen *)

(* Opgave 1 *)

type point = int * int
datatype rect = Rectangle of point * point

val point1 = (4, 5);
val point2 = (6, 4);
val rect1 = Rectangle((2, 2), (7, 4));
val rect2 = Rectangle((5, 1), (8, 3));
val rect3 = Rectangle((4, 5), (7, 7));

(* a *)
(* start with lowest left corner, and go clockwise *)
fun corners (Rectangle((x1, y1), (x2, y2))) = [(x1, y1), (x1, y2),
                                               (x2, y2), (x2, y1)];

(* b *)
fun inside (Rectangle((x1, y1), (x2, y2))) (q, z) = (q <= x2 andalso q >= x1)
                                                    andalso
                                                    (z <= y2 andalso z >= y1);

(* c *)
local
fun check (Rectangle((_), (_))) []      = false
  | check (Rectangle((a), (b))) (x::xs) = inside (Rectangle((a), (b))) x
                                          orelse
                                          check (Rectangle((a), (b))) xs
in
fun collisionRect (Rectangle((p1),(p2))) (Rectangle((pa),(pb)))=
    check (Rectangle((pa), (pb))) (corners (Rectangle((p1),(p2))))
    orelse
    check (Rectangle((p1), (p2))) (corners (Rectangle((pa),(pb))))
end;



(* Opgave 2 *)

(* eksicipli funktion af fibonacci *)
(* it is real -> real *)
fun fibber' n = ((Math.pow(1.0 + Math.sqrt(5.0), n) -
                 Math.pow(1.0 - Math.sqrt(5.0), n)) /
                (Math.pow(2.0, n) * Math.sqrt(5.0)))

(* Opgave 3 *)

val T3_1 = [3, 2, 5, 1, 7, 4];
(* a *)
local
  fun minimum [x]     = x
    | minimum []      = raise Empty
    | minimum (x::xs) = List.foldr (fn (xs, q) => Int.min(xs, q)) x xs

  fun maximum [x]     = x
    | maximum []      = raise Empty
    | maximum (x::xs) = List.foldr (fn (xs, q) => Int.max(xs, q)) x xs
in
fun minMax xs = ((minimum xs), (maximum xs))
end;

(* b *)

val d1 = [0.4, 0.3];
val d2 = [0.1, 0.2, 0.7];

local
  infix 6 **
  fun (x, x2) ** y = x * x2 + y : real;
in
fun dotProduct (xs, ys) = List.foldr (fn (xs, q) => xs ** q )
                                     0.0 (ListPair.zip(xs, ys))
end;

(* c *)
local
  fun plusMaker x 0 = []
    | plusMaker x n = x :: plusMaker (x+1) (n-1)

  fun minusMaker x 0 = []
    | minusMaker x n = List.rev (plusMaker x n)
in
fun fromTo (x, y) = case Int.compare(x, y) of
                     GREATER => (minusMaker y (x - y + 1))
                   | LESS    => (plusMaker x (y - x + 1))
                   | EQUAL   => [x]
end;


(* Opgave 4 *)

datatype stringTree = String of int * string
                    | Concat of int * stringTree * int * stringTree

(* a *)
fun fromString x = String((String.size x), x );

(* b *)

fun toString (String(x, y)) = y
  | toString (Concat(x, y, x2, y2)) = (toString y) ^ (toString y2);

(* c *)
(* "toString" funktion from 4.b *)
fun size x = (String.size(toString(x)));

(* d *)

infix 6 ^^
fun (String(x, y)) ^^ (String(q, s)) = Concat(x, String(x, y), q, String(q, s))
                                       : stringTree;

(* e *)
(* "size" funktion from 4.c *)
fun valid (String(x, y)) = String.size(y) = x
  | valid (Concat(x, x2, y, y2)) = (x = size x2) andalso
                                   (y = size y2)

(* f *)

fun sub (String(x, y)) n = if String.size(y) >= (n-1)
                           then List.nth((explode y), (n-1))
                           else raise Fail "muahahaha"
  | sub (Concat) = 
