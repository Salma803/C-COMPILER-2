#include <stdio.h>
void printf(char *f) { }

/* @requires n >= 0;
@assigns \nothing;
@ensures print n stars; */
void print_star(int n) {
  int i;
  for (i = 0; i < n; i = i + 1)
    printf("*");
  printf("\n");
}

/* @requires rien;
   @assigns *i, *j;
   @ensures swap the content of i and j; */
void swap(int *i, int *j) {
  int tmp;
  tmp = *i;
  *i = *j;
  *j = tmp;
}

int main() {
  int i, j;
  i = 42;
  j = 24;
  swap(&i, &j);
  print_star(42);
  return 0;
}
