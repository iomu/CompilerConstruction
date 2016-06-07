int main() {
	int x ;
	x = 6 ;
	int y ;
	y = x + 7 ;
	printInt(y);
	{
		int y ;
		y = 4 ;
		printInt(y);
		x = y ;
		printInt(x);
	}
	printInt(x);
	printInt(y);
}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

