(* samme måde at skrive ting op på *)
fun slt  (n, k) = if n < k then true else false;
fun slt (n, k) = not (n < k);

fun forbedr (x, g) = average (g, x/g);

fun fact n = n * fact (n-1);

fun godtnok (g, nyt_g) = abs (g - nyt_g) < 0.000001
