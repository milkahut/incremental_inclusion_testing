ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif


ifeq ($(strip $(wildcard Solvers)),)
	include Solvers.default
else
	include Solvers
endif


# OCAMLOPT=ocamlopt -cclib -static

all: satwrapper satsolvers zchaff minisat picosat pseudosat preprocessor externalsat tester tester2




satwrapper: $(OBJDIR)/satwrapper.cmi $(OBJDIR)/satwrapper.cmx

$(OBJDIR)/satwrapper.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/satwrapper.mli

$(OBJDIR)/satwrapper.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/satwrapper.ml


satsolvers: $(OBJDIR)/satsolvers.cmi $(OBJDIR)/satsolvers.cmx

$(OBJDIR)/satsolvers.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/satsolvers.mli

$(OBJDIR)/satsolvers.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/satsolvers.ml



ifeq "$(WITH_ZCHAFF)" "YES"

HASSAT=YES

zchaff: satwrapper satsolvers $(OBJDIR)/zchaff_ocaml_wrapper.o $(OBJDIR)/zchaff.cmi $(OBJDIR)/zchaff.cmx $(OBJDIR)/zchaffwrapper.cmi $(OBJDIR)/zchaffwrapper.cmx

$(OBJDIR)/zchaff_ocaml_wrapper.o:
	$(CPP) -c -g -I $(SRCDIR)/zchaff/backend -I $(OCAML_DIR) -o $@ $(SRCDIR)/zchaff/backend/zchaff_ocaml_wrapper.cpp

$(OBJDIR)/zchaff.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/zchaff/zchaff.mli

$(OBJDIR)/zchaff.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/zchaff/zchaff.ml

$(OBJDIR)/zchaffwrapper.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/zchaff/zchaffwrapper.mli

$(OBJDIR)/zchaffwrapper.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/zchaff/zchaffwrapper.ml

ZCHAFFOBJS=$(OBJDIR)/zchaff_ocaml_wrapper.o $(OBJDIR)/zchaff.cmx $(OBJDIR)/zchaffwrapper.cmx $(ZCHAFFDIR)/libsat.a 

EXTERNALSATOBJS += $(ZCHAFFOBJS)

else

zchaff:

endif



ifeq "$(WITH_MINISAT)" "YES"

HASSAT=YES

minisat: satwrapper satsolvers $(OBJDIR)/MiniSATWrap.o $(OBJDIR)/minisat.cmi $(OBJDIR)/minisat.cmx $(OBJDIR)/minisatwrapper.cmi $(OBJDIR)/minisatwrapper.cmx

#$(OBJDIR)/SimpSolver.o:
#	$(CPP) -c -I $(MINISATDIR) -I $(MINISATDIR)/mtl $(MINISATDIR)/simp/SimpSolver.cc -o $(OBJDIR)/SimpSolver.o

$(OBJDIR)/MiniSATWrap.o:
	$(CPP) -D__STDC_LIMIT_MACROS -c -I $(SRCDIR)/minisat/backend -I $(OCAML_DIR) -I $(MINISATDIR) -o $@ $(SRCDIR)/minisat/backend/MiniSATWrap.cc

$(OBJDIR)/minisat.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/minisat/minisat.mli

$(OBJDIR)/minisat.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/minisat/minisat.ml

$(OBJDIR)/minisatwrapper.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/minisat/minisatwrapper.mli

$(OBJDIR)/minisatwrapper.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/minisat/minisatwrapper.ml

MINISATOBJS=$(OBJDIR)/MiniSATWrap.o $(OBJDIR)/minisat.cmx $(OBJDIR)/minisatwrapper.cmx $(MINISATDIR)/simp/lib.a
MINISATFLAGS=-cclib -lz

EXTERNALSATOBJS += $(MINISATOBJS)
EXTERNALSATFLAGS += $(MINISATFLAGS)

else

minisat:

endif


ifeq "$(WITH_PICOSAT)" "YES"

HASSAT=YES

picosat: satwrapper satsolvers $(OBJDIR)/PicoSATWrap.o $(OBJDIR)/picosat.cmi $(OBJDIR)/picosat.cmx $(OBJDIR)/picosatwrapper.cmi $(OBJDIR)/picosatwrapper.cmx

$(OBJDIR)/PicoSATWrap.o:
	$(CPP) -c -g -I $(SRCDIR)/picosat/backend -I $(OCAML_DIR) -o $@ $(SRCDIR)/picosat/backend/PicoSATWrap.cc

$(OBJDIR)/picosat.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/picosat/picosat.mli

$(OBJDIR)/picosat.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/picosat/picosat.ml

$(OBJDIR)/picosatwrapper.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/picosat/picosatwrapper.mli

$(OBJDIR)/picosatwrapper.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/picosat/picosatwrapper.ml

PICOSATOBJS=$(OBJDIR)/PicoSATWrap.o $(OBJDIR)/picosat.cmx $(OBJDIR)/picosatwrapper.cmx $(PICOSATDIR)/libpicosat.a 

EXTERNALSATOBJS += $(PICOSATOBJS)

else

picosat:

endif


ifeq "$(HASSAT)" "YES"

CPPCOMPILER=-cc $(OCAMLOPTCPP)

endif


pseudosat: $(OBJDIR)/pseudosatwrapper.cmi $(OBJDIR)/pseudosatwrapper.cmx

$(OBJDIR)/pseudosatwrapper.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/pseudosat/pseudosatwrapper.mli

$(OBJDIR)/pseudosatwrapper.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/pseudosat/pseudosatwrapper.ml


preprocessor: $(OBJDIR)/preprocessor.cmi $(OBJDIR)/preprocessor.cmx

$(OBJDIR)/preprocessor.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/preprocessor/preprocessor.mli

$(OBJDIR)/preprocessor.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/preprocessor/preprocessor.ml


externalsat: $(OBJDIR)/satsolutionparserhelper.cmi $(OBJDIR)/externalsat.cmi $(SRCDIR)/externalsat/satsolutionparser.ml $(SRCDIR)/externalsat/satsolutionlexer.ml $(OBJDIR)/satsolutionparser.cmi $(OBJDIR)/satsolutionparser.cmx $(OBJDIR)/satsolutionlexer.cmx $(OBJDIR)/externalsat.cmx

$(OBJDIR)/satsolutionparserhelper.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/externalsat/satsolutionparserhelper.mli

$(SRCDIR)/externalsat/satsolutionparser.ml:
	$(OCAMLYACC) $(SRCDIR)/externalsat/satsolutionparser.mly

$(SRCDIR)/externalsat/satsolutionlexer.ml:
	$(OCAMLLEX) $(SRCDIR)/externalsat/satsolutionlexer.mll

$(OBJDIR)/satsolutionparser.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/externalsat/satsolutionparser.mli

$(OBJDIR)/satsolutionparser.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/externalsat/satsolutionparser.ml

$(OBJDIR)/satsolutionlexer.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/externalsat/satsolutionlexer.ml

$(OBJDIR)/externalsat.cmi:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/externalsat/externalsat.mli

$(OBJDIR)/externalsat.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ $(SRCDIR)/externalsat/externalsat.ml





$(OBJDIR)/tester.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ tester/tester.ml

tester: satwrapper satsolvers $(OBJDIR)/tester.cmx 
	$(OCAMLOPT) -o $(BINDIR)/tester $(CPPCOMPILER) $(OCAML_DIR)/libasmrun.a $(OBJDIR)/satwrapper.cmx $(OBJDIR)/satsolvers.cmx $(OBJDIR)/pseudosatwrapper.cmx $(OBJDIR)/preprocessor.cmx $(PICOSATOBJS) $(MINISATOBJS) $(ZCHAFFOBJS) $(OBJDIR)/tester.cmx


$(OBJDIR)/tester2.cmx:
	$(OCAMLOPT) -I $(OBJDIR) -c -o $@ tester/tester2.ml

tester2: satwrapper satsolvers $(EXTERNALSATOBJS) $(OBJDIR)/tester2.cmx
	$(OCAMLOPT) -o $(BINDIR)/tester2 $(CPPCOMPILER) $(EXTERNALSATFLAGS) $(OCAML_DIR)/libasmrun.a $(OBJDIR)/satwrapper.cmx $(OBJDIR)/satsolvers.cmx $(OBJDIR)/pseudosatwrapper.cmx $(OBJDIR)/preprocessor.cmx $(EXTERNALSATOBJS) $(OBJDIR)/tester2.cmx




clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.cmx $(OBJDIR)/*.cmi
	rm -f $(SRCDIR)/externalsat/satsolutionparser.ml $(SRCDIR)/externalsat/satsolutionparser.mli $(SRCDIR)/externalsat/satsolutionlexer.ml
	rm -f $(BINDIR)/tester $(BINDIR)/tester2
