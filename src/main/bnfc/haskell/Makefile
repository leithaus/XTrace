all:
	happy -gca Partrace.y
	alex -g Lextrace.x
	latex Doctrace.tex; dvips Doctrace.dvi -o Doctrace.ps
	ghc --make Testtrace.hs -o Testtrace
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doctrace.ps
distclean: clean
	-rm -f Doctrace.* Lextrace.* Partrace.* Layouttrace.* Skeltrace.* Printtrace.* Testtrace.* Abstrace.* Testtrace ErrM.* SharedString.* trace.dtd XMLtrace.* Makefile*

