(* David Paenphet Holberg Jørgensen *)

(* 1I1 *)
(* skriver den matematisk formel a^2 = 1/(a^1) vis brug for *)
fun powerRealInt (a, 0) = 1.0
  | powerRealInt (a, n) = if n > 0
                          then a * powerRealInt(a, n-1)
                          else 1.0 / powerRealInt(a, (abs(n)));

 (* powerREalInt (0.99, ~100) = 2.73199902643 *)

fun testEpsilon (a, b) = abs(b - a) < 0.000000001

val test_powerRealInt = testEpsilon(powerRealInt(0.99, ~100), 2.73199902643);


(* 1T2 *)
fun mystisk (0, n) = n
  | mystisk (m, n) =
      if m > n then mystisk (m - n, n)
      else if n > m then mystisk (m, n - m)
      else m;

(* Handkøre "1T2" mystisk. 
  ~~> mystisk (21, 56)
  ~~> mystisk (21, 56 - 21)
  ~~> mystisk (21, 35)
  ~~> mystisk (21, 35 - 21)
  ~~> mystisk (21, 14)
  ~~> mystisk (21 - 14, 14)
  ~~> mystisk (7, 14)
  ~~> mystisk (7, 14 - 7)
  ~~> mystisk (7, 7)
  ~~> mystisk (7)
*)

(* 1I3 *)
(* opretter funktion som laver mellem-rum for hvor mange "n" der er *)
fun space (0) = ""
  | space (n) = " " ^ pow(n-1);

(* laver output til string af den størrelse af tallet eller
   størrelsen af string "w" *)
fun rightAlign (n:int, w:int) = if   size(Int.toString(n)) >= w
                                then Int.toString(n)
                                else space(w - size(Int.toString(n)))
                                     ^ Int.toString(n);
