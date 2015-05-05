(* David Holberg PanPheant Jørgensen *)

(* 2I1 *)

(* læser hjælpe filer ind *)
;use "/Users/david155/Dropbox/DiKU/IP/2 uge - 1blok/uge2_distance.sml";
;use "/Users/david155/Dropbox/DiKU/IP/2 uge - 1blok/uge2_cphmarathon2014.sml";
;use "/Users/david155/Dropbox/DiKU/IP/2 uge - 1blok/uge2_dhl2014.sml";
;use "/Users/david155/Dropbox/DiKU/IP/2 uge - 1blok/uge2_kollegier.sml";

(* koordinat taget fra uge2_distance.sml *)
val dikuDis = (55.702028, 12.561144);

(* test med real tal *)
fun testEpsilon (a, b) = abs(b - a) < 0.000000001;

(* 2I1 *)

(* if, then, else, finder afstanden mellem 2 koordinater og listen
 * og tag det mindste ud
 *)
fun closestDistance (k, [])  = raise Empty
  | closestDistance (k, [x]) = distance(k, x)
  | closestDistance (k, q1::q2::qs) = if distance(k, q1) < distance(k, q2)
                                      then closestDistance(k, q1::qs)
                                      else closestDistance(k, q2::qs);

val test_2I1 = testEpsilon(closestDistance
                           (dikuDis, cphMarathon2014), 0.146488145221);

val test_2I1_Tom = closestDistance (dikuDis, []) =
                    false handle Empty => true;

(* 2I2 *)

(*
 * meget mergen til det 2I1, men kan tag flere oplysinger ud af en liste
 *)
fun closestPOI (k, [])  = raise Empty
  | closestPOI (k, [x]) = #2 x
  | closestPOI (k, ((q1, q11)::(q2, q22)::qs)) = if distance(k, q1) <
                                                    distance(k, q2)
                                 then closestPOI(k, (q1, q11)::qs)
                                 else closestPOI(k, (q2, q22)::qs);

val test_POI = closestPOI(dikuDis, kollegier) = "Sparekassenkollegiet";
(* 2I3 *)
(*
 * a' er en polymorf værdi, der elementet i listen er behøver ikke være "string"
 * men kan være hvad som helst, med min funktion #2 tag jeg hvad der er på anden
 * plads i listen lige gyldig hvad det er for et element på listen's plads
 *)




