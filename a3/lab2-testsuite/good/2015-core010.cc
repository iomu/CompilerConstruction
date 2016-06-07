int main() {
  printInt(fac(5));
  return 0 ;
}

int fac (int a) {
  int r;
  int n;
  r = 1;
  n = a;
  while (n > 0)
  {
    r = r * n;
    n = n - 1;
  }
  return r;
}

//void printInt(int x) { }
//void printDouble(double x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

