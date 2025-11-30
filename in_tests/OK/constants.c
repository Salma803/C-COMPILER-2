int main() {
  int i;
  double f; 
  char *s;
  i = 0;
  i = 1234567890;
  i = 012345670;
  i = 0x123456789ABCDEF0;
  i = 0X123456789abcdef0;
  i = -0;
  i = +1234567890;
  i = -012345670;
  i = +0x123456789ABCDEF0;
  i = -0X123456789abcdef0;
  f = 123.456;
  f = -123.;
  f = +.456;
  f = 123.456e+789;
  f = -123.e789;
  f = +.456e-789;
  f = 123e+789;
  f = -123e789;
  f = +456e-789;
  s = "Une chaine";
  s = "Trois" "chaines" "concatenees";
  s = "Une chaine"
"sur deux lignes";
  return 0;
}
