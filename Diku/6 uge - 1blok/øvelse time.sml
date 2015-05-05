

(**********)
(* Mandag *)
(**********)

(* 6M1 *)


TextIO.output(TextIO.stdOut, "Denne tekst indeholder ikke et linjeskift");

(* 6M2 *)

TextIO.output(TextIO.stdOut, "Simonsen\n");

fun issuffix x = x ^ "\r";

TextIO.output(TextIO.stdOut, issuffix "Simonsen2");

fun issuffixout x = TextIO.output(TextIO.stdOut, issuffix(x));

(* 6M3 *)

(*
val outstrm = TextIO.openOut "udgangsfil.txt";

TextIO.output (outstrm, "Dette er en prøve!∖n");

TextIO.flushOut outstrm;

TextIO.closeOut outstrm;
*)

(* 6M4 *)

val instrm = TextIO.openIn "indgangsfil.txt";

(***********)
(* Tirsdag *)
(***********)

(* 6T1 *)

fun listLine x = let val Torben = TextIO.openIn x
                     val allText = TextIO.inputAll Torben
                 in
                   String.fields(fn x => x = #"\n") allText
                   before
                   TextIO.closeIn Torben
                 end;

(* 6T2 *)
(*
fun listHelper [] = []
  | listHelper (x::xs) = explode(x) :: listHelper(xs);

fun listChars x = listHelper(listHelper(x));
*)

(**********)
(* Fredag *)
(**********)

(* 6F1 *)
(* old vision *)
fun lenght [] = 0
  | lenght (x::xs) = 1 + lenght(xs);

(* new vision *)
local
  fun lenghtHelper n [] = n
    | lenghtHelper n (x::xs) = lenghtHelper (n + 1) (xs)
in
  fun lenght xs = lenghtHelper 0 xs
end;

(* 6F2 *)

fun nth [] _ = raise Domain
  | nth (x::xs) 0 = x
  | nth (x::xs) i = nth (xs) (i-1);

(* old vision *)
fun sum (m, 0) = m
  | sum (m, n) = m + n + sum(m, n - 1);

(* new vision *)
local
  fun sumHelper (m, 0) acc = acc + m
    | sumHelper (m, n) acc = sumHelper (m, n - 1) (m + n + acc)
in
  fun sum' (m, n) = sumHelper (m, n) 0
end;
