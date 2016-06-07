int main ()
{
  int x = readInt () ;

  int d = x/2 ;

  while (d > 1) {
    if (d * (x/d) == x)
      printInt(d) ;
    else
      {}

    d-- ;
  }

}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

