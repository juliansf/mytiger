# Unix makefile for tigermain

HOME=/root
MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc

# Unix
REMOVE=rm -f
CD=cd
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#CD=cd
#MOVE=move
#EXEFILE=.exe

# Tiger Tree
ROOT=.
LEXER=./Lexer
PARSER=Parser
SEMANTIC=Semantic
CANON=Canonizer
CODEGEN=Codegen
MISC=Misc
BIN=bin
LIVENESS=Liveness
COLOR=Color

LOADPATH=-I $(LEXER) -I $(PARSER) -I $(MISC) -I $(SEMANTIC) -I $(CANON) -I $(CODEGEN) -I $(LIVENESS) -I $(COLOR)

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= \
	TigerLineNumber.uo \
	TigerProgName.uo \
	TigerAbs.uo \
	TigerTypes.uo \
	TigerError.uo \
	TigerUtils.uo \
	Parser.uo \
	Scanner.uo \
	tigertab.uo \
	tigerescap.uo \
	TigerMap.uo \
	TigerSet.uo \
	TigerStack.uo \
	TigerTemp.uo \
	TigerTree.uo \
	TigerAssem.uo \
	TigerSparcFrame.uo \
	TigerFrame.uo \
	TigerTranslate.uo \
	TigerEnv.uo \
	TigerSemant.uo \
	TigerCanon.uo \
	TigerSparcGen.uo \
	TigerCodegen.uo \
	TigerFlow.uo \
	TigerColor.uo \
	tigerpp.uo \
	tigermain.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	if ! test -d $(BIN); then mkdir $(BIN); fi; \
	$(MOSMLL) $(LOADPATH) -o $(BIN)/tiger$(EXEFILE) tigermain.uo

Parser.sml Parser.sig: $(PARSER)/Parser.y 
	$(MOSMLYACC) $(PARSER)/Parser.y

Scanner.sml: $(LEXER)/Scanner.lex
	$(MOSMLLEX) $(LEXER)/Scanner.lex

TigerAbs.sml: $(PARSER)/TigerAbs.sml

TigerLineNumber.sml: $(MISC)/TigerLineNumber.sml

tigerpp.sml: $(MISC)/tigerpp.sml

clean:
	$(REMOVE) Makefile.bak
	
	$(CD) $(PARSER);\
	$(REMOVE) Parser.output;\
	$(REMOVE) Parser.sig;\
	$(REMOVE) Parser.sml;\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(LEXER);\
	$(REMOVE) Scanner.sml;\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(MISC);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(SEMANTIC);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(CANON);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(CODEGEN);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(LIVENESS);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(CD) $(COLOR);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo
	
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $(LOADPATH) $<

TigerLineNumber.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerLineNumber.sml
TigerProgName.uo:
	$(MOSMLC) $(LOSDPATH) $(MISC)/TigerProgName.sml
TigerAbs.uo:
	$(MOSMLC) $(LOADPATH) $(PARSER)/TigerAbs.sml
TigerTypes.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTypes.sml
TigerError.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerError.sml
TigerUtils.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerUtils.sml	
Parser.uo: 
	$(MOSMLC) $(LOADPATH) $(PARSER)/Parser.sig $(PARSER)/Parser.sml
Scanner.uo: 
	$(MOSMLC) $(LOADPATH) $(LEXER)/Scanner.sml
tigermain.uo:
	$(MOSMLC) $(LOADPATH) tigermain.sml
tigertab.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigertab.sig $(MISC)/tigertab.sml
tigerescap.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigerescap.sig $(MISC)/tigerescap.sml
TigerMap.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerMap.sig $(MISC)/TigerMap.sml	
TigerSet.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerSet.sig $(MISC)/TigerSet.sml
TigerStack.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/TigerStack.sig $(MISC)/TigerStack.sml	
TigerTemp.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTemp.sig $(SEMANTIC)/TigerTemp.sml
TigerTree.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTree.sml
TigerSparcFrame.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerSparcFrame.sig $(SEMANTIC)/TigerSparcFrame.sml	
TigerFrame.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerFrame.sig $(SEMANTIC)/TigerFrame.sml	
TigerTranslate.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerTranslate.sig $(SEMANTIC)/TigerTranslate.sml
TigerEnv.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerEnv.sml
TigerSemant.uo:
	$(MOSMLC) $(LOADPATH) $(SEMANTIC)/TigerSemant.sig $(SEMANTIC)/TigerSemant.sml
TigerCanon.uo:
	$(MOSMLC) $(LOADPATH) $(CANON)/TigerCanon.sig $(CANON)/TigerCanon.sml
TigerAssem.uo:
	$(MOSMLC) $(LOADPATH) $(CODEGEN)/TigerAssem.sig $(CODEGEN)/TigerAssem.sml
TigerSparcGen.uo:
	$(MOSMLC) $(LOADPATH) $(CODEGEN)/TigerSparcGen.sig $(CODEGEN)/TigerSparcGen.sml
TigerCodegen.uo:
	$(MOSMLC) $(LOADPATH) $(CODEGEN)/TigerCodegen.sig $(CODEGEN)/TigerCodegen.sml
TigerFlow.uo:
	$(MOSMLC) $(LOADPATH) $(LIVENESS)/TigerFlow.sml
TigerColor.uo:
	$(MOSMLC) $(LOADPATH) $(COLOR)/TigerColor.sig $(COLOR)/TigerColor.sml
tigerpp.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigerpp.sml
	
#.sml.uo:
#	$(MOSMLC) $<

depend: Parser.sml Scanner.sml

### DO NOT DELETE THIS LINE
