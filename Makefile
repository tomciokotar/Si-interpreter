all:
	happy -gca ParSi.y
	alex -g LexSi.x
	ghc --make Main.hs -o interpreter
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocSi.ps
distclean: clean
	-rm -f DocSi.* LexSi.* ParSi.* LayoutSi.* SkelSi.* PrintSi.* TestSi.* AbsSi.* TestSi ErrM.* SharedString.* Si.dtd XMLSi.* Makefile*

