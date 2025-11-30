int main() {
  int x;
  {
    int y;
    y = 2;
    x = y - 3;
  }
  x = y - x;
  return 0;
}
