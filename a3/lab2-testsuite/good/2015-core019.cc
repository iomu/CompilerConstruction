int main() {
  int i = 78;
  {
    int i = 1;
    printInt(i);
  }
  printInt(i);
  while (i > 76) {
    i--;
    printInt(i);
   int i = 7;
   printInt(i);
  }
  printInt(i);
  if (i > 4) {
    int i = 4;
    printInt(i);
  } else {

  } 
  printInt(i);
  return 0 ;

}

//void printInt(int x) { }
//void printDouble(double x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

