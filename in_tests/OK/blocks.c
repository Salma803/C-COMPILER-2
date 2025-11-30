int main() {
  int x;
  {
    int y;
    {
      double x;
      y = 1;
      {
	int *y;
	char z;
	x = 2.;
	y = &z;
      }
      y = 3;
      x = 1e32;
    }
    x = 2;
    {
      float x;
      y = 1 + (int) (x - 2.3);
    }
  }
  return x;
}
