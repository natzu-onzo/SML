(* Group assignment # 5:
 * - Philip Falck
 * - Alexander Roed Thorup
 * - David Jørgensen
 * - Asbjørn Michaelsen
 *)




(* Assignment 5G1 *)

(* Type declarations *)
datatype primaryColours = Black | Red | Green | Blue
                        | White | Cyan | Magenta | Yellow;

(* Converts a primaryColours to a (R, G, B) tuple *)
fun toRGB (Black)   = (  0,   0,   0)
  | toRGB (Red)     = (255,   0,   0)
  | toRGB (Green)   = (  0, 255,   0)
  | toRGB (Blue)    = (  0,   0, 255)
  | toRGB (White)   = (255, 255, 255)
  | toRGB (Cyan)    = (  0, 255, 255)
  | toRGB (Magenta) = (255,   0, 255)
  | toRGB (Yellow)  = (255, 255,   0);

(* Unit testing *)
val test_toRGB_01 =
    toRGB(Black) = (0, 0, 0);
val test_toRGB_02 =
    toRGB(Green) = (0, 255, 0);




(* Assignment 5G2 *)

(* Type declarations *)
type point = real * real;

datatype 'col figure = Circle of 'col * point * real
                     | Rectangle of 'col * point * point
                     | Over of 'col figure * 'col figure;

(* Changes the format of the colour by applying a transformation function *)
fun reColour (Circle(c, center, radius)) f  = Circle(f(c), center, radius)
  | reColour (Rectangle(c, crn_a, crn_b)) f = Rectangle(f(c), crn_a, crn_b)
  | reColour (Over(f1, f2)) f               = Over(reColour f1 f, reColour f2 f);

(* Unit testing *)
local
    val redCircleOverBlueSquare =
        Over(
            Circle(Red, (0.0, 0.0), 1.8),
            Rectangle(Blue, (~1.5, ~1.5), (1.5, 1.5))
        )
in
    val test_reColour_01 =
        reColour redCircleOverBlueSquare toRGB = Over(
            Circle((255, 0, 0), (0.0, 0.0), 1.8),
            Rectangle((0, 0, 255), (~1.5, ~1.5), (1.5, 1.5))
        )
    
    val test_reColour_02 =
        reColour (Circle(Cyan, (0.0, 0.0), 1.8)) (toRGB) =
			Circle((0, 255, 255), (0.0, 0.0), 1.8)
			
	val test_reColour_03 =
        reColour (Rectangle(Black, (0.0, 0.0), (1.0, 1.0))) (toRGB) =
			Rectangle((0, 0, 0), (0.0, 0.0), (1.0, 1.0))
end;




(* Assignment 5G3 *)

local
    (* Checks if the point is contained within a figure *)
    fun isContained (Circle(_, (a, b), radius), (x, y))    =
            (x - a) * (x - a) + (y - b) * (y - b) <= (radius * radius)
      | isContained (Rectangle(_, (a, b), (c, d)), (x, y)) =
            (x >= a andalso x <= c andalso y >= b andalso y <= d)
      | isContained (Over(_, _), (_, _))                   =
            raise Fail "Prevent warning message"
in
    (* Checks if a point contains a colour and returns SOME colour or NONE *)
    fun colourOf (Circle(colour, center, radius)) (point)  =
            if (isContained(Circle(colour, center, radius), point))
            then SOME colour
            else NONE
      | colourOf (Rectangle(colour, crn_a, crn_b)) (point) =
            if (isContained(Rectangle(colour, crn_a, crn_b), point))
            then SOME colour
            else NONE
      | colourOf (Over(f1, f2)) (point)                    =
            if (colourOf (f1) (point) <> NONE)
            then colourOf (f1) (point)
            else colourOf (f2) (point)
end;

(* Unit testing *)
local
    val test_figure_01 =
        Over (
            Circle(Red, (0.0, 0.0), 1.0),
            Rectangle(Blue, (0.0, 0.0), (2.0, 2.0))
        )

    val test_figure_02 =
        Circle(Yellow, (0.0, 0.0), 1.0)

    val test_figure_03 =
        Rectangle(Blue, (~1.5, ~1.5), (1.5, 1.5))
in
    (* Testing Over, with some in first *)
    val test_colourOf_01 =
        colourOf (test_figure_01) (0.5, 0.5) = SOME Red
    (* Testing Over, with some in second *)
    val test_colourOf_02 =
        colourOf (test_figure_01) (1.5, 1.5) = SOME Blue
    (* Testing Over, with none *)
    val test_colourOf_03 =
        colourOf (test_figure_01) (3.5, 3.5) = NONE
    (* Testing circle some *)
    val test_colourOf_04 =
        colourOf (test_figure_02) (0.5, 0.5) = SOME Yellow
    (* Testing circle none *)
    val test_colourOf_05 =
        colourOf (test_figure_02) (2.5, 2.5) = NONE
    (* Testing rectangle some *)
    val test_colourOf_06 =
        colourOf (test_figure_03) (0.5, 0.5) = SOME Blue
    (* Testing rectangle none *)
    val test_colourOf_07 =
        colourOf (test_figure_03) (2.5, 2.5) = NONE
end;




(* Assignment 5G4 *)

local
	(* ?? *)
    fun resolve (SOME _) = true
      | resolve (NONE)   = false
in
	(* ?? *)
    fun hasAColour (Circle(content)) (point)    =
            resolve(colourOf(Circle(content)) (point))
      | hasAColour (Rectangle(content)) (point) =
            resolve(colourOf(Rectangle(content)) (point))
      | hasAColour (Over(f1, f2)) (point)       =
            hasAColour (f1) (point) orelse hasAColour (f2) (point)
end;


