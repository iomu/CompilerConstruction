int main () {
	int n, i;
	n = readInt();
	i = 2;
	while (i <= n) {
		bool iPrime = true;
		int j = 2;
		while (j*j <= i && iPrime) {
			if ((i / j) * j == i) {
				iPrime = false;
			} else {}
			j++;
		}

		if (iPrime && (n / i) * i == n) {
			printInt(i);
			n = n / i;
		} else { 
			i++; 
		}
	}

	return 0;
}
void   printInt(int x) {}       // print an integer and a newline in standard output
    void   printDouble(double x) {}  // print a double and a newline in standard output
    int    readInt() {}             // read an integer from standard input
    double readDouble() {}           // read a double from standard input

