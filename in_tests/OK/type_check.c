int main() {
  int x;
  double y;
  char *z;
  x = x + 2 * x - 4 % x / 1;
  y = y + 2. * y - 4e0 / 1.0;
  z = &x;
  {
    x = (x > 2) && (y < 2.3) || !(z != "coucou");
    z[x - 3] += x;
  }
  if (z == &z[2]);
  while (!x);
  for(;;y += (float) 1);
  return 42;
}
