int main() {
  int j = 4;

  while (j < 6){ int i = 0 ; i++ ; printInt(i); j++; }
  if (j < 7) j++ ; else { j--; }

  printInt(j);
  return j;
}

//void printInt(int x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

