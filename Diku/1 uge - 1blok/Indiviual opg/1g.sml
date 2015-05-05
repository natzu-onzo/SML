(* 1G1 *)
fun disk (a, b, c) = (b * b) - 4.0 * a * c;

(* andengrads ligning *)
fun solve2 (a : real, b : real, c : real) =
    if (a <> 0.0 andalso disk(a, b, c) >= 0.0)
    then ( (~ b - Math.sqrt( disk(a, b, c))) / (2.0 * a),
           (~ b + Math.sqrt( disk(a, b, c))) / (2.0 * a) )
    else raise Fail "d er negative eller a er ligmed 0"

  (* 1G2
     solve2 (1.0, 2.0, 3.0)
     fungere fint uden problemer samt for de rigtgi værdier.
  *)

  (* 1G2
     solve2 (2.0, 2.0, 3.0)
     her giver vores diskriminemt "<= 0" så derfor villige vores
     program give en "raise Fail"
  *)

(* 1G4 *)
fun power2 (a, 0) = 1
  | power2 (a, n) = a * power2(a, n-1);

fun powerNew (a, 0) = 1
  | powerNew (a, n) = if (n mod 2 = 1) then a * power2(a, n-1)
                      else power2( powerNew (a, n div 2), 2);

(* 1G5 *)
fun power2antal (a, 0) = 1
  | power2antal (a, n) = 1 + power2antal(a, n-1);

  (* om skriver PowerNew til at +1 til vores power istedet for at gange *)
fun powerAntal (a, 0) = 0
  | powerAntal (a, n) = if (n mod 2 = 1) then 1 + powerAntal(a, n-1)
                      else powerAntal(a, n div 2) + 2

  (* køre power-Antal funtioner og giver output til powerCounter *)
fun powerCounter (a, n) = (powerNew(a, n), powerAntal(a, n));


