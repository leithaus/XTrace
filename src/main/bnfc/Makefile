JAVAC = javac
JAVAC_FLAGS = -sourcepath .

JAVA = java
JAVA_FLAGS =

CUP = java_cup.Main
CUPFLAGS = -nopositions -expect 100

JLEX = JLex.Main

LATEX = latex
DVIPS = dvips

all: test trace.ps

test: absyn com/biosimilarity/validation/model/trace/Yylex.class com/biosimilarity/validation/model/trace/PrettyPrinter.class com/biosimilarity/validation/model/trace/Test.class com/biosimilarity/validation/model/trace/ComposVisitor.class com/biosimilarity/validation/model/trace/AbstractVisitor.class com/biosimilarity/validation/model/trace/FoldVisitor.class com/biosimilarity/validation/model/trace/AllVisitor.class com/biosimilarity/validation/model/trace/parser.class com/biosimilarity/validation/model/trace/sym.class com/biosimilarity/validation/model/trace/Test.class

.PHONY: absyn

%.class: %.java
	${JAVAC} ${JAVAC_FLAGS} $^

absyn: com/biosimilarity/validation/model/trace/Absyn/Expression.java com/biosimilarity/validation/model/trace/Absyn/Sequence.java com/biosimilarity/validation/model/trace/Absyn/Application.java com/biosimilarity/validation/model/trace/Absyn/Mention.java com/biosimilarity/validation/model/trace/Absyn/Value.java com/biosimilarity/validation/model/trace/Absyn/Abstraction.java com/biosimilarity/validation/model/trace/Absyn/Stipulation.java com/biosimilarity/validation/model/trace/Absyn/VariableExpr.java com/biosimilarity/validation/model/trace/Absyn/Transcription.java com/biosimilarity/validation/model/trace/Absyn/AtomLiteral.java com/biosimilarity/validation/model/trace/Absyn/ValueExpr.java com/biosimilarity/validation/model/trace/Absyn/Numeric.java com/biosimilarity/validation/model/trace/Absyn/ListVariableExpr.java
	${JAVAC} ${JAVAC_FLAGS} $^

com/biosimilarity/validation/model/trace/Yylex.java: com/biosimilarity/validation/model/trace/Yylex
	${JAVA} ${JAVA_FLAGS} ${JLEX} com/biosimilarity/validation/model/trace/Yylex

com/biosimilarity/validation/model/trace/sym.java com/biosimilarity/validation/model/trace/parser.java: com/biosimilarity/validation/model/trace/trace.cup
	${JAVA} ${JAVA_FLAGS} ${CUP} ${CUPFLAGS} com/biosimilarity/validation/model/trace/trace.cup
	mv sym.java parser.java com/biosimilarity/validation/model/trace/

com/biosimilarity/validation/model/trace/Yylex.class: com/biosimilarity/validation/model/trace/Yylex.java com/biosimilarity/validation/model/trace/sym.java

com/biosimilarity/validation/model/trace/sym.class: com/biosimilarity/validation/model/trace/sym.java

com/biosimilarity/validation/model/trace/parser.class: com/biosimilarity/validation/model/trace/parser.java com/biosimilarity/validation/model/trace/sym.java

com/biosimilarity/validation/model/trace/PrettyPrinter.class: com/biosimilarity/validation/model/trace/PrettyPrinter.java

trace.dvi: trace.tex
	${LATEX} trace.tex

trace.ps: trace.dvi
	${DVIPS} trace.dvi -o trace.ps

clean:
	 rm -f com/biosimilarity/validation/model/trace/Absyn/*.class com/biosimilarity/validation/model/trace/*.class
	 rm -f .dvi trace.aux trace.log trace.ps  *.class

vclean:
	 rm -f com/biosimilarity/validation/model/trace/Absyn/Expression.java com/biosimilarity/validation/model/trace/Absyn/Sequence.java com/biosimilarity/validation/model/trace/Absyn/Application.java com/biosimilarity/validation/model/trace/Absyn/Mention.java com/biosimilarity/validation/model/trace/Absyn/Value.java com/biosimilarity/validation/model/trace/Absyn/Abstraction.java com/biosimilarity/validation/model/trace/Absyn/Stipulation.java com/biosimilarity/validation/model/trace/Absyn/VariableExpr.java com/biosimilarity/validation/model/trace/Absyn/Transcription.java com/biosimilarity/validation/model/trace/Absyn/AtomLiteral.java com/biosimilarity/validation/model/trace/Absyn/ValueExpr.java com/biosimilarity/validation/model/trace/Absyn/Numeric.java com/biosimilarity/validation/model/trace/Absyn/ListVariableExpr.java com/biosimilarity/validation/model/trace/Absyn/Expression.class com/biosimilarity/validation/model/trace/Absyn/Sequence.class com/biosimilarity/validation/model/trace/Absyn/Application.class com/biosimilarity/validation/model/trace/Absyn/Mention.class com/biosimilarity/validation/model/trace/Absyn/Value.class com/biosimilarity/validation/model/trace/Absyn/Abstraction.class com/biosimilarity/validation/model/trace/Absyn/Stipulation.class com/biosimilarity/validation/model/trace/Absyn/VariableExpr.class com/biosimilarity/validation/model/trace/Absyn/Transcription.class com/biosimilarity/validation/model/trace/Absyn/AtomLiteral.class com/biosimilarity/validation/model/trace/Absyn/ValueExpr.class com/biosimilarity/validation/model/trace/Absyn/Numeric.class com/biosimilarity/validation/model/trace/Absyn/ListVariableExpr.class
	 rm -f com/biosimilarity/validation/model/trace/Absyn/*.class
	 rmdir com/biosimilarity/validation/model/trace/Absyn/
	 rm -f trace.tex trace.dvi trace.aux trace.log trace.ps 
	 rm -f com/biosimilarity/validation/model/trace/Yylex com/biosimilarity/validation/model/trace/trace.cup com/biosimilarity/validation/model/trace/Yylex.java com/biosimilarity/validation/model/trace/VisitSkel.java com/biosimilarity/validation/model/trace/ComposVisitor.java com/biosimilarity/validation/model/trace/AbstractVisitor.java com/biosimilarity/validation/model/trace/FoldVisitor.java com/biosimilarity/validation/model/trace/AllVisitor.java com/biosimilarity/validation/model/trace/PrettyPrinter.java com/biosimilarity/validation/model/trace/Skeleton.java com/biosimilarity/validation/model/trace/Test.java com/biosimilarity/validation/model/trace/sym.java com/biosimilarity/validation/model/trace/parser.java com/biosimilarity/validation/model/trace/*.class
	 rm -f Makefile
	 rmdir -p com/biosimilarity/validation/model/trace/

