(* Group assignment # 2:
 * - Philip Falck
 * - Alexander Roed Thorup
 * - David Jørgensen
 * - Asbjørn Michaelsen
 *)

;use "2014_08_09 IP_uge_2_distance.sml";
;use "2014_08_09 IP_uge_2_cphmarathon2014.sml";


(* Floating point evaluation function *)
fun epsilon (a, b) =
    (abs(b - a) < 0.000000001);

(* Variable for unit testing *)
val storebaelt = [((55.336145, 10.990714), 0),
				  ((55.349420, 11.095690), 1500000),
				  ((55.336145, 10.990714), 3600000)];




(* Assignment 2G1 *)

(* Returns the total distance [km] from a list of (real * real) coordinates *)
fun totalDistance (a::b::c) = distance(a, b) + totalDistance(b::c)
  | totalDistance [_]       = 0.0
  | totalDistance []        = raise Empty;

(*
    val test_totalDistance_01 =
        epsilon(totalDistance(cphMarathon2014),42.9686446838)
    val test_totalDistance_02 =
        epsilon(totalDistance([(55.66639000,12.57621000)]), 0.0)
    val test_totalDistance_03 =
        (totalDistance([]); false) handle Empty => true | _ => false
*)




(* Assignment 2G2 *)

(* Converts an int miliseconds to real hours *)
fun ms2Hour (n) =
    (real(n) / 3600000.0);

(* Returns a real array with the average speed between each coordinate pair *)
fun speeds ((p1, t1)::(p2, t2)::c) = [distance(p1, p2) / ms2Hour(t2 - t1)]
                                     @ speeds((p2, t2)::c)
  | speeds [_]                     = []
  | speeds []                      = raise Empty;

(*
    val test_speeds_01 = speeds([((55.336145, 10.990714), 0)]) = []
    val test_speeds_02 = (speeds([]); false) handle Empty => true | _ => false
*)




(* Assignment 2G3 *)

(* Sums up all elements of a real list *)
fun sum (a::b) = a + sum(b)
  | sum []	   = 0.0;

(* Returns the average of a list of reals *)
fun averageAll [] = raise Empty
  | averageAll a  = sum(a) / real(length(a));

(*
    val test_averageAll_01 = epsilon(averageAll([1.0, 2.0, 1.0, 2.0]), 1.5)
    val test_averageAll_02 = (averageAll([]); false)
                             handle Empty => true | _ => false
*)




(* Assignment 2G4 *)

(* Returns the average speed of a ((real * real) * int) list *)
fun averageSpeed []  = raise Empty
  | averageSpeed [_] = raise Domain
  | averageSpeed a 	 = averageAll(speeds(a))

(*
    val test_averageSpeed_01 = (averageSpeed([]); false)
                               handle Empty => true | _ => false
    val test_averageSpeed_02 = epsilon(averageSpeed(storebaelt), 13.9886997339)
    val test_averageSpeed_03 = (averageSpeed([((1.0, 1.0), 0)]); false)
                               handle Domain => true | _ => false
*)




(* Assignment 2G5 *)

(* Functionality:
 *
 * (1) "fun rightAlign (n,0)" case is not required as (n, 0) -> Int.toString(n)
 * 	   and this is a perfectly valid input, so it shouldn't raise an exception.
 *     Declaring the clause is incorrect. The case should be removed, or the
 *     cat gets it!
 *
 * (2) "if String.size s = w" does not take into account when size s > w and it
 *     will send the program into a infinit loop, an infiiiiiiiiinite looooooop
 *     I tell you, if you try. Change "=" to ">=" to prevent this.
 *
 * Documentation:
 *
 * (1) Comments over multible lines are harder to read, keep comments shorter
 *	   and now pause, look left, wait two second, look back. See how comments
 *     can be written over multible lines by using the (* a \n b *) syntax.
 *
 * Style:
 *
 * (1) In the line "if String.size s = w then s else rightAl..." should either
 * 	   be broken up into multible lines as 'if \n then \n else', to increase
 *	   the readability. If you keep it as a single line, at least parenthesis
 *	   the if (expression) to make it readable.
 *
 * (2) Consistent code line after line is not wrong from a syntax standpoint,
 *     but it makes the readability wrong. Use blank lines (line breaks) to
 *     split up code into segments.
 *
 * Unit testing:
 *
 * (1)
 *)




(*
mangler test af n, m : m < n
*)





