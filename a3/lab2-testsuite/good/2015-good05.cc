int main () {
	int lo,hi,mx ;

        lo = 1 ;
	hi = lo ;

        //mx = 5000000 ; //readInt () ;
        mx = readInt () ;

       	printInt(lo) ;
	while (hi < mx) {
	    printInt(hi) ;
	    hi = lo + hi ;
	    lo = hi - lo ;
	}
    }
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

