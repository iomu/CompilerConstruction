all: dist/build/codegen/codegen

dist/build/codegen/codegen:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	cabal configure
	cabal build

clean:
	rm -rf dist
	rm -f *.log *.aux *.hi *.o *.dvi
	rm -f DocCPP.ps

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*


