(* David Paenphet Holberg Jørgensen *)

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

(* checks if there is poin (q, z) in is the rectangle *)
fun inside (Rectangle((x1, y1), (x2, y2))) (q, z) = (q <= x2 andalso q >= x1)
                                                    andalso
                                                    (z <= y2 andalso z >= y1);

(* c *)

(* checks if two rectangles overlaps *)
local
(* using 1,b funcktion to help *)
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

(* a *)

(* alternativ fibonacci with iterativ funktion *)
local
  fun fibb 0 x y z = x
    | fibb n x y z = fibb (n-1) y z (x+z)
in
fun fibber' n = fibb n 0 1 2
end;

(* Opgave 3 *)


(* a *)

(* finding the smallest and biggest number in a list *)
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

val A2 = minMax [3, 2, 5, 1, 7, 4] = (1, 7);

(* b *)

local
  infix 6 **
  fun (x, x2) ** y = x * x2 + y : real;
in
fun dotProduct (xs, ys) = List.foldr (fn (xs, q) => xs ** q )
                                     0.0 (ListPair.zip(xs, ys))
end;

val d1 = [0.4, 0.3];
val d2 = [0.1, 0.2, 0.7];

val B2 = dotProduct (d1, d2) = 0.1;

(* c *)

(* making a list from the smallest number to the biggest number *)
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

(* putting a "string" in to the tree *)
fun fromString x = String((String.size x), x );

(* b *)

(* taking the "string" out of the tree *)
fun toString (String(x, y)) = y
  | toString (Concat(x, y, x2, y2)) = (toString y) ^ (toString y2);

(* c *)

(* the size of the "string" in a tree *)
(* "toString" funktion from 4.b *)
fun size x = (String.size(toString(x)));

(* d *)

(* new infix to make leaves to a tree *)
local
infix 6 ^^
in
fun op^^ ((String(x, y)), (String(z, w))) = Concat(x, String(x, y),
                                                   z, String(z, w))
  | op^^ ((String(x, y)), _)  = String(x, y)
  | op^^ ((Concat(x, y, z, w)), a) = Concat(size(y), op^^ (a, y), z, w)
end;


(* e *)

(* cheking the structure of the tree*)
(* "size" funktion from 4.c *)
fun valid (String(x, y)) = String.size(y) = x
  | valid (Concat(x, x2, y, y2)) = (x = size x2) andalso
                                   (y = size y2)

(* f *)

(* taking a char out of the tree *)
fun sub (String(x, y)) n = if x >= n
                           then List.nth((explode(toString(String(x, y)))), n)
                           else raise Subscript

  | sub (Concat(x, y, z, q)) n = if size(Concat(x, y, z, q)) >= n
                                 then List.nth(
                                     (explode(toString(Concat(x, y, z, q)))), n)
                                 else raise Subscript;

(* g *)
(*
local
fun subT (String(x, y)) z = String(z, implode((List.drop(explode(y), z))))
  | subT (Concat(x, y, v, w)) z = raise Fail "matching making..."
in
fun subTree (Concat(x, y, z, w)) (a, b) = Concat(size(y), (subTree y (a, b)),
                                                 size(w), (subTree w (a, b)))
  | subTree (String(x, y)) (a, b) = if x < a
                                    then subT (String(x, y)) (b-(b-x))
                                    else subT (String(x, y)) b
end;
*)

(* h *)

(* print a tree out to a outstream *)
local
  fun print' (String(_, y)) = y
    | print' (Concat(_, y, _, w)) = (print'(y) ;  print'(w))

in
fun printTree ((Concat(x, y, z, w)), str) = (TextIO.output(str, (print'(y)));
                                     TextIO.output(str, (print'(w))) ;
                                     TextIO.output(str, "\n"))
  | printTree ((String(x, y)), str) = TextIO.output(str, y)
end;

(* Opgave 5 *)

(* a *)

(* cheks if a string is a substring of an other string *)
local
fun number x (y::ys) qs acc = if (string.isSubstring x (implode qs))
                              then acc
                              else (number x ys (qs @ [y]) (acc + 1))
  | number x [] qs acc = 0

in
fun search x y =if (string.isSubstring x y)
                then SOME (number x (explode y) []  (0 - (String.size x)))
                else NONE
end;

(* b *)

(* search in a giving txt file *)
local
(* laant fra vejledende loesninger 6T1 *)
  fun input filename =
      let
        val name = TextIO.openIn(filename);
        val tekst = TextIO.inputAll name;
      in
        String.fields (fn x => #"\n" = x) tekst
        before TextIO.closeIn name
      end

  fun number x (y::ys) qs acc = if (string.isSubstring x (implode qs))
                                then acc
                                else (number x ys (qs @ [y]) (acc + 1))
    | number x [] qs acc = 0

  fun send x (y::ys) a = if (string.isSubstring x y)
                         then SOME (a, (number x (explode y) []  (0 - (String.size x))))
                         else send x ys (a+1)
    | send x [] a = raise Fail "mach making"
in
fun searchFile x y = send x (input y) 1
end;


(* Opgave 6 *)

(* a *)

(* instets a int in to a list of lists *)
local
fun putL x [] = [[x]]
  | putL x ys = [x :: ys]

fun makeL 0 x  = [[x]]
  | makeL n x  = [[]] @ makeL (n-1) x
in
fun insertG (n, x, xs) = if n >= 0 andalso List.length(xs) >= n
                         then (List.take(xs, n)) @ (putL x (List.nth(xs, n)))
                              @ (List.drop(xs, n))
                         else if n >= 0 andalso List.length(xs) <= n
                         then xs @ (makeL (n - List.length(xs)) x)
                         else raise Domain
end;

(* b *)

(*
fun group f (x::xs) = (insertG(List.length((x::xs)),(foldl f (x::xs)), (x::xs)))
*)

(*Opgave 7*)

signature Queue =
 sig

   (* Typen af en kø med elementer af typen ’a. *)
   type 'a queue

   (* Den tomme kø. *)
   val empty : 'a queue

   (* Tilføj et element bagest i køen. *)
   val enqueue : 'a * 'a queue -> 'a queue

   (* Tag et element ud fra starten af køen. *)
   (* Returner elementet og resten af køen. *)
   (* Rejs undtagelsen Empty, hvis køen er tom. *)
   val dequeue : 'a queue -> 'a * 'a queue
 end

(* a *)
(*
structure Queue :> Queue = 
struct
 type Queue = 'a queue

end
*)
