(* David Paenphet Holberg Jørgensen *)

(* winning sudoku *)
val sv = [[#"5", #"3", #"4", #"6", #"7", #"8", #"9", #"1", #"2"],
          [#"6", #"7", #"2", #"1", #"9", #"5", #"3", #"4", #"8"],
          [#"1", #"9", #"8", #"3", #"4", #"2", #"5", #"6", #"7"],
          [#"8", #"5", #"9", #"7", #"6", #"1", #"4", #"2", #"3"],
          [#"4", #"2", #"6", #"8", #"5", #"3", #"7", #"9", #"1"],
          [#"7", #"1", #"3", #"9", #"2", #"4", #"8", #"5", #"6"],
          [#"9", #"6", #"1", #"5", #"3", #"7", #"2", #"8", #"4"],
          [#"2", #"8", #"7", #"4", #"1", #"9", #"6", #"3", #"5"],
          [#"3", #"4", #"5", #"2", #"8", #"6", #"1", #"7", #"9"]];
(* column fail*)
val sf1 = [[#"5", #"3", #"4", #"6", #"7", #"8", #"9", #"1", #"2"],
           [#"6", #"7", #"2", #"1", #"9", #"5", #"3", #"4", #"9"],
           [#"1", #"9", #"8", #"3", #"4", #"2", #"5", #"6", #"7"],
           [#"8", #"5", #"9", #"7", #"6", #"1", #"4", #"2", #"3"],
           [#"4", #"2", #"6", #"8", #"5", #"3", #"7", #"9", #"1"],
           [#"7", #"1", #"3", #"9", #"2", #"4", #"8", #"5", #"6"],
           [#"9", #"6", #"1", #"5", #"3", #"7", #"2", #"8", #"4"],
           [#"2", #"8", #"7", #"4", #"1", #"9", #"6", #"3", #"5"],
           [#"3", #"4", #"5", #"2", #"8", #"6", #"1", #"7", #"9"]];
(* row fail *)
val sf2 = [[#"5", #"3", #"4", #"6", #"7", #"8", #"9", #"1", #"2"],
           [#"6", #"7", #"2", #"1", #"9", #"5", #"3", #"4", #"9"],
           [#"1", #"9", #"8", #"3", #"4", #"2", #"5", #"6", #"7"],
           [#"8", #"5", #"9", #"7", #"6", #"1", #"4", #"2", #"3"],
           [#"4", #"2", #"6", #"8", #"5", #"3", #"7", #"9", #"1"],
           [#"7", #"1", #"3", #"9", #"2", #"4", #"8", #"5", #"6"],
           [#"9", #"6", #"1", #"5", #"3", #"7", #"2", #"8", #"4"],
           [#"2", #"8", #"7", #"4", #"1", #"9", #"6", #"3", #"5"],
           [#"3", #"9", #"5", #"2", #"8", #"6", #"1", #"7", #"9"]];
(* region fail *)
val sf3 = [[#"5", #"3", #"4", #"6", #"7", #"8", #"9", #"1", #"2"],
           [#"6", #"7", #"2", #"1", #"9", #"5", #"3", #"4", #"9"],
           [#"1", #"9", #"8", #"3", #"4", #"2", #"5", #"6", #"7"],
           [#"8", #"5", #"9", #"7", #"6", #"1", #"4", #"2", #"3"],
           [#"4", #"2", #"6", #"8", #"5", #"3", #"7", #"9", #"1"],
           [#"7", #"1", #"3", #"9", #"2", #"4", #"8", #"5", #"6"],
           [#"9", #"6", #"1", #"5", #"3", #"7", #"2", #"8", #"4"],
           [#"2", #"8", #"7", #"4", #"1", #"9", #"6", #"9", #"5"],
           [#"3", #"4", #"5", #"2", #"8", #"6", #"1", #"7", #"9"]];



(*******)
(* 6I1 *)
(*******)

infix member

fun x member [] = false
  | x member (y::ys) = x = y orelse x member ys

(* checking if a list is in a other list *)
fun subList [] _  = true
  | subList _ []  = false
  | subList (x::xs) ys = x member ys andalso subList xs ys


val dex = [3,4,5,6,7,8];
val xed = [8,7,6,5,4,3];
val exd = [3,4,5,6,7,9];
val a = [1,2,3,4,5];
val b = [1,2,3,4,5,6];

val I1_test_1 = subList dex xed = true;
val I1_test_2 = subList xed dex = true;
val I1_test_3 = subList a b = true;
val I1_test_4 = subList b a = false;

(*******)
(* 6I2 *)
(*******)

(* taking a column out of a char list list (sudoku) *)
fun column n xs = map (fn x => List.nth(x, n)) xs;

val I2_test_1 = column 2 sv =
                [#"4", #"2", #"8", #"9", #"6", #"3", #"1", #"7", #"5"];
val I2_test_2 = column 4 sv =
                [#"7", #"9", #"4", #"6", #"5", #"2", #"3", #"1", #"8"];


(*******)
(* 6I3 *)
(*******)

(* subList kan checker om der en list som er i anden liste, som er lige så stor
 *eller større, der med kan vi checker om alle tallende fra 1 til 9 er i en liste
 *i vores sudoku liste, der en sudoku liste skal indholde alle tal, for at være
 *løst. Vi har column som kan kan lave en liste fra en list liste, der med kan
 *vi lave en list over talende i en column, og der med bruge subList igen til at
 *tjekke om alle tallende igen er der også, der man også skal bruge dem til at
 *vinde sudoku
 *)

(*******)
(* 6I4 *)
(*******)

(* checking a row and a column in a charr list list (sudoku)
 * have the right set of numbers
 *)
local
  val s = [#"1",#"2",#"3",#"4",#"5",#"6",#"7",#"8",#"9"]

  fun subCheck _ _ 9     = true
    | subCheck [] _ _    = raise Domain
    | subCheck xs ys (n) = if (subList ys (List.nth(xs, n)))
                         then subCheck xs ys (n+1)
                         else false

  fun colCheck _ _ 9     = true
    | colCheck [] _ _    = raise Domain
    | colCheck xs ys (n) = if (subList ys (column n xs))
                         then colCheck xs ys (n+1)
                         else false
in
  fun rowcolChecker xs = if subCheck xs s 0 andalso colCheck xs s 0
                         then true
                         else false
end;

val I4_test_1 = rowcolChecker sv = true;
val I4_test_2 = rowcolChecker sf1 = false;
val I4_test_3 = rowcolChecker sf2 = false;

(*******)
(* 6I5 *)
(*******)


(* checking a row, column and region in a char list list (sudoku) *)
local
  val s = [#"1",#"2",#"3",#"4",#"5",#"6",#"7",#"8",#"9"]

  fun regiMaker (xs) (q) =
      [List.nth(List.nth(xs, q div 3 * 3 + 0), q mod 3 * 3 + 0),
       List.nth(List.nth(xs, q div 3 * 3 + 0), q mod 3 * 3 + 1),
       List.nth(List.nth(xs, q div 3 * 3 + 0), q mod 3 * 3 + 2),
       List.nth(List.nth(xs, q div 3 * 3 + 1), q mod 3 * 3 + 0),
       List.nth(List.nth(xs, q div 3 * 3 + 1), q mod 3 * 3 + 1),
       List.nth(List.nth(xs, q div 3 * 3 + 1), q mod 3 * 3 + 2),
       List.nth(List.nth(xs, q div 3 * 3 + 2), q mod 3 * 3 + 0),
       List.nth(List.nth(xs, q div 3 * 3 + 2), q mod 3 * 3 + 1),
       List.nth(List.nth(xs, q div 3 * 3 + 2), q mod 3 * 3 + 2)]

  fun regiCheck _ _ 8   = true
    | regiCheck [] _ _  = raise Domain
    | regiCheck xs ys n = if (subList (ys) (regiMaker xs n))
                          then regiCheck xs ys (n+1)
                          else false
in
fun checkSudoku xs = if rowcolChecker xs andalso regiCheck xs s 0
                     then true
                     else false

end;

val I5_test_1 = checkSudoku sv  = true;
val I5_test_2 = checkSudoku sf1 = false;
val I5_test_3 = checkSudoku sf2 = false;
val I5_test_4 = checkSudoku sf3 = false;
