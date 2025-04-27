int to_next_int_index(real x) {
  if (x<0) {
    reject("to_next_int_index: x must be non-negative");
  }
  real x_rounded = round(x);
  int i = 1;
  while (x_rounded > 1) {
    x_rounded -= 1;
    i += 1;
  }
  return(i);
}