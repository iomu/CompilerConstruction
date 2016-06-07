bool foo(int x) {
    if (x == 0) {
	return false;
    } else {}

    bool b = true;

    printInt(x);

    return b;
}

void printBool(bool b) {
	if (false) {}
  else

  if (b) {
	  printInt(0);
  } else {
	  printInt(1);
  }

}

int main() {
  printBool(foo(42));
  printBool(foo(0));
  return 0;
}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

