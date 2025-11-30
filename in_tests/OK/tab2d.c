#include <stdio.h>
#include <stdlib.h>
void *malloc(int s) { }
void *calloc(int n, int s) { }
void free(void *p) {}

/*@ requires w >= 0 et h >= 0
  assigns *a;
  ensures initialise a avec un tableau de largeur w et de hauteur h */
void init(double **a, int w, int h) {
  *a = (double *) malloc(h * w * sizeof (double));
}

double get(double *a, int w, int i, int j) { // remplace t[i][j]
  return a[i * w + j];
}

void set(double *a, int w, int i, int j, double v) { // remplace t[i][j] = v
  a[i * w + j] = v;
}

int main() {
  double **t;
  double *t2;
  double *t3;
  int i, j;
    t = (double **) malloc(4 * sizeof (double *));
  for (i = 0; i < 4; i += 1)
    t[i] = (double *) calloc(6, sizeof (double));
  for (i = 0; i < 4; i += 1)
    for (j = 0; j < 6; j += 1)
      t[i][j] = (double) (i + j) / 2.;
  for (i = 0; i < 4; i += 1)
    free((void *) t[i]);
  free((void *) t);
  t2 = (double *) malloc(4 * 6 * sizeof (double));
  for (i = 0; i < 4; i += 1)
    for (j = 0; j < 6; j += 1)
      t2[i * 6 + j] = (double) (i + j) / 2.;
  free((void *)t2);
  init(&t3, 6, 4);
  for (i = 0; i < 4; i += 1)
    for (j = 0; j < 6; j += 1)
    set(t3, 6, i, j, (double) (i + j) / 2.);
  return 0;
}
