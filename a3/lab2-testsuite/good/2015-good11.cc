//read numbers until 0 is read, and print their average

int main () 
{
  int sum = 0 ;
  int num = 0 ;
  int x ;
  while ((x = readInt()) != 0) {
    sum = sum + x ;
    num++ ;
  }
  printInt(sum/num) ;

}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

