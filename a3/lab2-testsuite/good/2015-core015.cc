/* parity of positive integers by recursion */

int main () {
  printInt(ev(17)) ;
  return 0 ;
}

int ev (int y) {
  int e ;
  if (y > 0)
    e = ev (y-2) ;
  else
    if (y < 0)
      e = 0 ;
    else
      e = 1 ;
  return e ;
}

//void printInt(int x) { }
//void printDouble(double x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

