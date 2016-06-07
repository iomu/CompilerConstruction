int main () {
	int i = 1;
	printInt(i);
	true || i++ != 45;
	printInt(i);
	false || i++ >= 0;
	printInt(i);
	true && i++ < 0;
	printInt(i);
	false && i++ > 0;
	printInt(i);
	int j = 0;
	if (34 < 6 && j < 0) {
		printInt(i);
	} else { 
		printInt (42); 
	}
}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

