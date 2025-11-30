int main() {
  int x;
  for (x = 0; x < 10; x += 1)
    x -= 0;
  for (; x < 20; x += 1)
    x -= 0;
  for (x = 0; x < 10; )
    x += 1;
  for (; x != 10; );
  for (x = 0; ; x += 1) 
    if (x == 100) return 0;
  return 0;
}
