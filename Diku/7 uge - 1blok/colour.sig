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
