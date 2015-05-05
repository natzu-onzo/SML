datatype selection = Pick of int
                   | Choose of real * selection * selection;

val s1234 = Choose (0.3, Choose (0.4, Pick 1, Pick 2),
                    Choose (0.2 , Pick 3 , Pick 4));



(**********)
(* Mandag *)
(**********)

(* 5M1 *)


fun fairDie1 1 = Pick(1)
  | fairDie1 n = Choose(1.0 / (real n), Pick(n), fairDie1(n - 1));

(* 5M2 *)

fun isValid (Pick _)           = true
  | isValid (Choose (p, s, t)) = 0.0 <= p andalso p <= 1.0
                                 andalso isValid(s) andalso isValid(t);

val test_5M2_1 = isValid(Pick 1);
val test_5M2_2 = isValid(Choose (0.2, Pick 2, Choose(0.4, Pick 2, Pick 5)));
val test_5M2_1 = not (isValid(Choose(5.0, Pick 2, Pick 1)));

(* 5M3 *)

;load "Random";

fun randomPick (Pick n) _             = n
  | randomPick (Choose (p, s, t)) gen = if Random.random gen <= p
                                        then randomPick s gen
                                        else randomPick t gen;

(* 5M4 *)

fun probabilityOf (Pick n) x           = if n = x then 1.0 else 0.0
  | probabilityOf (Choose (p, s, t)) x = p * probabilityOf s x +
                                         (1.0 - p) * probabilityOf t x;

(* 5M5 *)

fun average (Pick n)            = real n
  | average (Choose(p, s, t)) = p * average s + (1.0 - p) * average t;

val test_5M5_1 = average s1234 = 3.14;

(***********)
(* Tirsdag *)
(***********)



datatype leafTree = Leaf of int
                  | Branch of leafTree * leafTree;

(* 5T1 *)

fun treeSum (Leaf n) = n
  | treeSum (Branch(v, h)) = treeSum(v) + treeSum(h);

(* 5T2 *)

fun treeMax (Leaf n) = n
  | treeMax (Branch(v, h)) = Int.max(treeMax v, treeMax h);

(* 5T3 *)

fun tree2List (Leaf n) = [n]
  | tree2List (Branch(v, h)) = (tree2List v) @ (tree2List h);

(* 5T4 *)

(*
 * Tegn
 *)

(* 5T6 *)

fun mirrorTree (Leaf n) = Leaf n
  | mirrorTree (Branch(v, h)) = Branch(mirrorTree h, mirrorTree v);


(**********)
(* fredag *)
(**********)


(* 5F1 *)
datatype 'a generalTree = Node of 'a * 'a generalTree list

fun Nodes (Node(_, xs)) = 1 + NodesList(xs)

and NodesList [] = 0
  | NodesList (x::xs) = Nodes x + NodesList xs;

(* 5F2 *)

fun nodes (Node(_, xs)) = 1 + foldl(fn (e, acc) => nodes(e) + acc) 0 xs;

(* 5F3 *)

fun preOrder (Node(a, xs)) = a :: preOrderList(xs)

and preOrderList [] = []
  | preOrderList (x::xs) = preOrder x @ preOrderList xs;

fun preOder1 (Node(a, xs)) = a :: List.concat(map(preOrder1) xs);
