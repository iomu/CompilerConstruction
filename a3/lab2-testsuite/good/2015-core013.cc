int main() {
  printBool(test(0-1) && test(0));
  printBool(test(0-2) || test(1));
  printBool(test(3) && test(0-5) && true);
  printBool(test(3) || test(0-5) && true);
  printBool(true);
  printBool(false);
  return 0 ;

}

void printBool(bool b) {
  if (b) {
  } else {
 }
}

bool test(int i) {
  return i > 0;
}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

