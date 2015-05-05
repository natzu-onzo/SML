fun power (x, 0) = 1
  | power (x, n) = x * power (x, n-1);

fun fact 0 = 1
  | fact n = n * fact(n-1);

fun fact2 n = if n = 0
              then 1
              else n * fact(n-1);

fun power2 (x, n) = if n = 0
                    then = 1
                    else = x * power (x, n-1);
