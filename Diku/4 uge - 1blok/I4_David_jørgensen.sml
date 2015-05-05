(* David Paenphet Holberg Jørgensen *)

(*******)
(* 4I1 *)
(*******)

datatype ('a, 'b) either = Left of 'a | Right of 'b;


val ListeLR = [Left 2, Right 6, Left 3];
val ListeL = [Left 2];

(* checking if it is Left or Right, and put in a right list *)
fun partEither ([]) = ([],[])
  | partEither (x::xs) = let val (l, r) = partEither(xs)
                          in case (x) of
                                 Left y => (y::l, r)
                              | Right y => (l, y::r)
                         end;

val test_4I1_a = partEither (ListeLR) = ([2, 3], [6]);
val test_4I1_b = partEither ([]) = ([], []);
val test_4I1_c = partEither (ListeL) = ([2], []);
(*******)
(* 4I2 *)
(*******)

;
type username = string
type message = int * username * string
datatype status = Online | Away | Offline
datatype chatuser = User of username * status
datatype chatroom = PrivateChat of username * message list
                  | GroupRoom of username list * message list;

val userlist = [ User (" Sus ",    Offline ), User (" Mike ",  Online ),
                 User (" Markus ", Online ),  User (" Emil ",  Online ),
                 User (" Jan ",    Offline),  User (" Niels ", Offline)];

(* mix rooms *)
val room1 = [PrivateChat (" Sus ", [(793980034, " Sus ", " Hello ! " )]),
              GroupRoom ([" Mike ", " Markus ", " Emil "],
                        [(793983657, " Mike ", " Hopla ! " )])];
(*2x Private rooms*)
val room2 = [PrivateChat (" Sus ", [(793980034, " Sus ", " Hello ! " )]),
            PrivateChat(" Markus ",[(793980040, " Markus ", " heyyyy " )])];

(*2x Group rooms *)
val room3 = [ GroupRoom ([" Mike ", " Markus ", " Emil "],
                         [(793983657, " Mike ", " Hopla ! " )]),
                GroupRoom ([" Sus ", " Niels ", " Jan "],
                           [(793983658, " Mike ", " Hopla ! " )])];

local

(* sammenligner *)
fun onlineUser ([], navn) = false
  | onlineUser (User(x, y)::xs, navn) = case navn = x andalso
                                               (y = Online orelse y = Away) of
                                            true  => true
                                          | false => onlineUser(xs, navn)

(* takes one name from the usernamelist to onlineUser *)
fun onlineList ([], navn ) = false
  | onlineList (_, []) = false
  | onlineList (user, x::xs) = case onlineUser(user, x) of
                                            true  => true
                                          | false => onlineList(user, xs)


in
fun removeOffline (users, []) = []
  | removeOffline ([], users) = []
  | removeOffline (users,(PrivateChat(x, y)::ys)) = if (onlineUser(users, x))
                                                    then PrivateChat (x, y) ::
                                                         removeOffline(users, ys)
                                                    else removeOffline(users, ys)
 | removeOffline (users,(GroupRoom(xs, y)::ys))    = if (onlineList(users, xs))
                                                    then GroupRoom(xs, y) ::
                                                         removeOffline(users, ys)
                                                    else removeOffline(users, ys)
end;

val test_4I2_1 = removeOffline(userlist, room1) = [GroupRoom
                                                       ([" Mike ", " Markus ",
                                                         " Emil "],
                                                        [(793983657, " Mike ",
                                                          " Hopla ! ")])];

val test_4I2_2 = removeOffline(userlist, room2) = [PrivateChat
                                                       (" Markus ",
                                                        [(793980040, " Markus ",
                                                          " heyyyy ")])];

val test_4I2_3 = removeOffline(userlist, room3) = [GroupRoom
                                                       ([" Mike ", " Markus ",
                                                         " Emil "],
                                                        [(793983657, " Mike ",
                                                          " Hopla ! ")])];

val test_4I2_4 = removeOffline(userlist, []) = [];
val test_4I2_5 = removeOffline([], room1) = [];
(*******)
(* 4I3 *)
(*******)
;
(*opgaven siger at ouput skal være en "int option" men har valgt at lave den
 * til en "list" efter som der kan være flere chatrums i samme liste af chatrum
 *)
 (* den fejler, mulighed for at du kan sige hvor fejlen ligger? *)
local
fun date ([], x) = x
  | date ((y, _, _)::ys, x) = if y < x
                      then y
                      else date(ys, y)
in

fun newestMessage [] = []
  | newestMessage (PrivateChat(_, (y, _, _)::ys)::qs) = if y < date(ys, y)
                                               then y :: newestMessage(qs)
                                               else newestMessage(qs)
  | newestMessage (GroupRoom(_, (y, _, _)::ys)::qs) = if y < date(ys, y)
                                               then y:: newestMessage(qs)
                                               else newestMessage(qs)
end;
