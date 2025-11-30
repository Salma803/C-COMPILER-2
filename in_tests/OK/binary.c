void print_int(int i) {}
void print_string(char *s) {}

void *empty_stack() {}

int is_empty(void *s) {}

void push(int v, void **s) {}

int pop(void **s) {}

void print_binary(int n) {
  void *s;
  s = empty_stack();
  while (n > 0) {
    push(n % 2, &s);
    n = n / 2;
  }
  while (!is_empty(s)) 
    print_int(pop(&s));
}

int main() {
  print_binary(42);
  print_string("\n");
  return 0;
}
