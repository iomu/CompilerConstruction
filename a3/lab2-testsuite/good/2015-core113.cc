int main() {
  printInt(f(45));
  printInt(f(450));
  return 0;
}

int f(int x) {
  int y ;
  if (x < 100) {
    int x = 91;
    y = x;
  } else {
    y = x;
  }
  return y ;
}

//void printInt(int x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

