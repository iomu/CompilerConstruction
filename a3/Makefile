all:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	ghc --make tccpp.hs -o tccpp

clean:
	-rm -f tccpp
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCPP.ps

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*
