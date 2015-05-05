structure Colour :> Colour =
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
