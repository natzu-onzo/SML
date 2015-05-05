(* David Paenphet Holberg Jørgensen *)

(*  *)
;use "InstagraML.sml";
;use "Effects.sml";

val torben = InstagraML.readBMP ("torben.bmp");

open InstagraML;

(*********)
(** 3I1 **)
(*********)

(* sætte dem i local der de skal bruge en parameter (img) *)
local
fun ZE img  = zoomEffect(img);
fun FE img  = fishEffect(img);
fun WE img  = whirlEffect(img);
fun R45 img = rotate45degrees(img);
fun IC img  = invertColours(img);
in
val listeE = [ZE, FE, WE,
                R45, IC];
end

(* liste effecter på billede *)
fun effects (img,  xs) = foldl (fn (x, acc) => x(acc)) img xs;

(* printer billede *)
val printTorben = writeBMP("multiEffectTorben.bmp", effects(torben, listeE));

 (*********)
 (** 3I2 **)
 (*********)

(*
 * effects;
 * val 'b it = fn : 'b * ('b -> 'b) list -> 'b
 *
 * -----
 *
 * funktion af en polymof funktion 'b, som indholder en funktion, som i vores
 * tilfælle tag et billede ('b) og
 * tag en liste med funktioner (('b -> 'b) liste), som så bliver sat sammen
 * så billede i ('b) bliver abejdet i funktionerne i listen ('b * ('b -> 'b)).
 * så der ved output kommer et billede ('b) igen som inholder alle de effecter
 * som der var i vores funktion liste.
 *)

 (*********)
 (** 3I3 **)
 (*********)

(* liste over butikker *)
val openHourse = [(16*60,     4*60, "La Luna Pizzaria"), (* 16:00 - 04:00 *)
                  ( 8*60,    22*60, "Netto"),            (* 08:00 - 22:00 *)
                  (    0, 23*60+59, "Stene Apotek")]     (* 00:00 - 23:59 *)

(* sammeligner tider *)
fun openStores (x::xs) 0 = raise Domain
  | openStores [] t = []
  | openStores ((x1, x2, x3)::xs) t = if t > x1 andalso t < x2
                                      then x3 :: openStores xs t
                                      else if t < x1 andalso t > x2
                                      then x3 :: openStores xs t
                                      else if t > 1439 orelse t < 0
                                      then raise Domain
                                      else openStores xs t;

val testOS_1 = openStores openHourse 400 = ["La Luna Pizzaria", "Stene Apotek"];
val testOS_2 = openStores openHourse 1200 = ["Netto", "Stene Apotek"];
(* val testOS_1 = openStores openHourse 1450 false handle Domain => true; *)
(*hvorfor virekr denne test ikke?*)
