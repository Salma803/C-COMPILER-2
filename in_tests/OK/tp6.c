void *malloc(int s) {}
void print_int(int i) {}
void print_newline() {}


int **init(int s) {
  int **r;
  int i;
  r = (int **) malloc(s * sizeof (int *));
  for (i = 0; i < s; i += 1)
    r[i] = (int *) malloc(s * sizeof (int));
  return r;
}

void print_matrix(int **m, int s) {
  int i, j;
  for (i = 0; i < s; i += 1) {
    for (j = 0; j < s; j += 1)
      print_int (m[i][j]);
    print_newline();
  }
}

void print_addresses_matrix(int **m, int s) {
  int i, j;
  for (i = 0; i < s; i += 1) {
    for (j = 0; j < s; j += 1)
      print_int ((int) &m[i][j]);
    print_newline();
  }
}

