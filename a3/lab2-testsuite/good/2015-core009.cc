// Calling functions which take zero parameters

int main() {
 int x = foo();
 printInt(x);
 return 0 ;

}

int foo() {
 return 10;
}

//void printInt(int x) { }
//void printDouble(double x) { }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

