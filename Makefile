ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif


XMLLIGHTDIR=aux/xml-light
PGSOLVERLIBS=$(TCSLIBDIR)/obj/tcslib.cmxa $(PGSOLVERDIR)/obj/libpgsolver.cmxa 
LIBS = $(PGSOLVERLIBS) $(XMLLIGHTDIR)/xml-light.cmxa unix.cmxa

MODULES=misc.cmx \
	ba_parser.cmx \
	ba_lexer.cmx \
	nba.cmx \
	game.cmx \
	staticgame.cmx \
	dynamicgame.cmx \
	pebblegame.cmx \
	gff_tools.cmx \
	benchmarking.cmx

INTERFACES=$(MODULES:.cmx=.cmi)

INCLUDES=-I $(PGSOLVERDIR)/obj -I $(XMLLIGHTDIR)

iit: pgsolver xmllight $(INTERFACES) $(MODULES) iit.cmx 
	$(OCAMLOPT) $(INCLUDES) $(LIBS) $(MODULES) iit.cmx -o iit

xmllight:
	make -C $(XMLLIGHTDIR) opt

pgsolver: 
	make -C $(PGSOLVERDIR) library

%.cmx: %.ml
	$(OCAMLOPT) $(INCLUDES) -annot -c -o $@ $<

ba_lexer.cmi: ba_lexer.ml ba_parser.cmi
	$(OCAMLOPT) -c ba_lexer.ml

%.cmi: %.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

clean:
	rm -rf *.cmx *.cmi *.annot 

veryclean: clean
	rm -rf *.cmo *.o *~
	make -C $(XMLLIGHTDIR) clean
	make -C $(PGSOLVERDIR) cleanall


depend:
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

include .depend
