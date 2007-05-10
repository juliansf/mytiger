# User Preferences
#WORKINGDIR=/Users/Julian/Lcc/5/Compiladores/Compiler/tiger

# Unix makefile for tigermain

HOME=/usr/share
MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

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
MISC=Misc

LOADPATH=-I $(LEXER) -I $(PARSER) -I $(SEMANTIC) -I $(MISC)

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= \
	tigernlin.uo \
	tigerabs.uo \
	tigergrm.uo \
	tigerlex.uo \
	tigerpp.uo \
	tigermain.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) $(LOADPATH) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: $(PARSER)/tigergrm.y 
	$(MOSMLYACC) $(PARSER)/tigergrm.y

tigerlex.sml: $(LEXER)/tigerlex.lex
	$(MOSMLLEX) $(LEXER)/tigerlex.lex

tigerabs.sml: $(PARSER)/tigerabs.sml

tigernlin.sml: $(MISC)/tigernlin.sml

tigerpp.sml: $(MISC)/tigerpp.sml

clean:
	$(REMOVE) Makefile.bak
	
	$(CD) $(PARSER);\
	$(REMOVE) tigergrm.output;\
	$(REMOVE) tigergrm.sig;\
	$(REMOVE) tigergrm.sml;\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo;\
	
	$(CD) $(LEXER);\
	$(REMOVE) tigerlex.sml;\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo;\
	
	$(CD) $(MISC);\
	$(REMOVE) *.ui;\
	$(REMOVE) *.uo;\
	
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $(LOADPATH) $<

tigerabs.uo:
	$(MOSMLC) $(LOADPATH) $(PARSER)/tigerabs.sml

tigergrm.uo: 
	$(MOSMLC) $(LOADPATH) $(PARSER)/tigergrm.sig $(PARSER)/tigergrm.sml
tigerlex.uo: 
	$(MOSMLC) $(LOADPATH) $(LEXER)/tigerlex.sml
tigermain.uo:
	$(MOSMLC) $(LOADPATH) tigermain.sml
tigernlin.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigernlin.sml
tigerpp.uo:
	$(MOSMLC) $(LOADPATH) $(MISC)/tigerpp.sml
	
#.sml.uo:
#	$(MOSMLC) $<

depend: tigernlin.sml tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
