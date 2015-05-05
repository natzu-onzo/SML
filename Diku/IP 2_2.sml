fun pow (s, 0) = ""
  | pow (s, n) = s ^ pow(s, n-1);
