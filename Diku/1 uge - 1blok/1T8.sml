fun power 0 = 0
  | power n = n + power(n - 1);

fun po (m, 0) = 0
  | po (m, n) = (m + n) + po(m, n-1);



val test_po_2_3 = po (2, 3) = 12
