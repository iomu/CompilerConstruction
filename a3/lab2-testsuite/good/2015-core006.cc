// Declaration of multiple variables of the same type in one statement:

int main () {
  int x, y;
  x = 45;
  y = 36 + 67;
  printInt(x);
  printInt(y);
  return 0 ;

}

//void printInt(int x) { }
//void printDouble(double x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

