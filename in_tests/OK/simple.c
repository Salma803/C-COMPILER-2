#include <stdio.h>

void printf(char *format, int i, double d) {}

int main() {
  int x;
  float y;
  x = 3;
  y = (double) (2 * x) / 4.;
  printf("x vaut %d et y vaut %f\n", x, y);
  return 0; }
  
