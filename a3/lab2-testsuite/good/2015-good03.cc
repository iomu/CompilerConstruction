int main ()
{
  int arg = readInt() ;
  int ret = 1 ;

  int i = 1 ;

  while (i < arg + 1) {
    ret = i * ret ;
    ++i ;
  }
  printInt(ret) ;

}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

