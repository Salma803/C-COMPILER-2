//#include <stdio.h>
//#include <stdlib.h>
//#define DIM 3
int *malloc(int i) {}
int DIM;
int *NULL;
void printf(char *format, int i) {}
void free(int *p) {}

//typedef int vector[DIM];

/*@ requires nothing;
  assigns nothing;
  ensures returns a vector containing only v */
int *vector_of(int v) {
  int *real_res;
  int i;
  real_res = malloc(DIM * sizeof (int));
  if (real_res != NULL)
    for (i = 0; i < DIM; i += 1)
      real_res[i] = v;
  return real_res;
}
/*
void print_vector(int *v) {
  int i;
  for (i = 0; i < DIM; i += 1)
    printf("%d ", v[i]);
}

int main() {
  int *t;
  int *u;
  t = NULL;
  u = NULL;
  t = vector_of(42);
  if (t == NULL) {
    printf("Cannot allocate %d memory for t\n", DIM);
    return 12;
  }
  printf("%d\n", t[0]);
  printf("%d\n", t[1]);
  printf("%d\n", t[2]);
  u = vector_of(26);
  if (u == NULL) {
    printf("Cannot allocate %d memory for u\n", DIM);
    return 12;
  }
  printf("%d\n", u[0]);
  printf("%d\n", u[1]);
  printf("%d\n", u[2]);
  u[1] = 2;
  free(u);
  u = NULL;
  printf("%d\n", u[0]);
  printf("%d\n", u[1]);
  printf("%d\n", u[2]);
  print_vector(t);
  return 0;
}
*/
