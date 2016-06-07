int main () 
{
  int i = readInt() ; // 5

  printInt(i) ;   //5
  printInt(i++) ; //5
  printInt(i) ;   //6
  printInt(++i) ; //7
  printInt(i) ;   //7

}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

