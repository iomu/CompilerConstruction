/* parity of positive integers by loop */

int main () {
  int y = 17;
  while (y > 0)
    y = y - 2;
  if (y < 0) {
    printInt(0);
    return 0 ;
    }
  else {
    printInt(1);
    return 0 ;
    }
}

//void printInt(int x) { }
//void printDouble(double x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

