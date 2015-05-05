(**********)
(* Mandag *)
(**********)

(* 7M1 *)
signature Colour =
sig
  type colour

  val black   : colour
  val white   : colour
  val red     : colour
  val green   : colour
  val blue    : colour
  val cyan    : colour
  val magenta : colour
  val yellow  : colour

  val toRGB      : colour -> int * int * int
  val fromRGB    : int * int * int -> colour
  val compliment : colour -> colour
end;

(* 7M2 *)

structure RGB : Colour =
struct
 type colour = int * int * int

 val black   = (  0,   0,   0)
 val white   = (255, 255, 255)
 val red     = (255,   0,   0)
 val green   = (  0, 255,   0)
 val blue    = (  0,   0, 255)
 val cyan    = (  0, 255, 255)
 val magenta = (255,   0, 255)
 val yellow  = (255, 255,   0)

 fun toRGB c = c
 fun fromRGB c = c
 fun compliment (r, g, b) = (255 - r, 255 - g, 255 - b)
end;

(* 7M3 *)

structure PrimaryColours :> Colour =
struct
 datatype primaryColours = Black | White | Red | Green
                           | Blue | Cyan | Magenta | Yellow

 type colour = primaryColours
 val black   = Black
 val white   = White
 val red     = Red
 val green   = Green
 val blue    = Blue
 val cyan    = Cyan
 val magenta = Magenta
 val yellow  = Yellow

 fun rund x = if x <= 127
              then 0
              else 255

 fun toRGB (Black)   = (  0,   0,   0)
   | toRGB (Red)     = (255,   0,   0)
   | toRGB (Green)   = (  0, 255,   0)
   | toRGB (Blue)    = (  0,   0, 255)
   | toRGB (White)   = (255, 255, 255)
   | toRGB (Cyan)    = (  0, 255, 255)
   | toRGB (Magenta) = (255,   0, 255)
   | toRGB (Yellow)  = (255, 255,   0)

 fun fromRGB (r, g, b) = case (rund r, rund g, rund b) of
                            (  0,   0,   0) => Black
                          | (255, 255, 255) => White
                          | (255,   0,   0) => Red
                          | (  0, 255,   0) => Green
                          | (  0,   0, 255) => Blue
                          | (  0, 255, 255) => Cyan
                          | (255,   0, 255) => Magenta
                          | (255, 255,   0) => Yellow

 fun compliment Black   = White
   | compliment White   = Black
   | compliment Red     = Cyan
   | compliment Cyan    = Red
   | compliment Green   = Yellow
   | compliment Yellow  = Green
   | compliment Blue    = Magenta
   | compliment Magenta = Blue

end;

(* 7M5 *)

signature ExtendedColour =
sig
  include Colour
  val +++ : colour * colour -> colour
  val *** : real * colour -> colour
end;

(* 7M6 *)

structure ExtendedRGB : ExtendedColour =
struct
 open RGB;

 fun rund x = Int.max(0, Int.min(255, x))

 fun +++ ((r, b, g), (r1, b1, g1)) = (rund(r + r1),
                                       rund(b + b1),
                                       rund(g + g1))

 fun *** (n, (r, b, g)) = (rund (Real.round (n * real r)),
                             rund (Real.round (n * real b)),
                             rund (Real.round (n * real g)))
end;

structure  ExtendedPrimaryColours : ExtendedColour =
struct
 open PrimaryColours

 fun +++ (x1, x2) = fromRGB (ExtendedRGB.+++(toRGB x1, toRGB x2))
 fun *** (n, c) = fromRGB (ExtendedRGB.***(n, toRGB c))

end;
