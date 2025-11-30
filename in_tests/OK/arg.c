//#include <stdio.h>
void print_string(char *f, char *a) {}
void fprint_int(void *c, char *f, int a) {}
void print(char *f) {}
void fwrite(char *t, int s, int n, void *c) {}
void *fopen(char *n, char *d) {}
void fclose(void *f) {}
void fgets(char *t, int s, void *c) {}

int main(int argc, char **argv) {
  int i; char *buffer; void *f;
  print_string("Nom du programme : %s\n", argv[0]);
  if (argc < 2) {
    print("Needs an argument.\n");
    return 3;
  }
  f = fopen(argv[1], "w");
  fprint_int(f, "Une valeur : %d\n", 42);

  fwrite(buffer, sizeof (double), 42, f);
  fclose(f);
  f = fopen(argv[1], "r");
  fgets(buffer, 127, f);
  fclose(f);
  print_string("%s", buffer);
  
  
  return 0;
}
  
  
