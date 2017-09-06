ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif

INCLUDES=-I $(SRCDIR) -I $(OBJDIR) -I $(OCAML_DIR)

MODULES_DATA=$(OBJDIR)/tcsbasedata.cmx \
	$(OBJDIR)/tcsmaths.cmx \
	$(OBJDIR)/tcsarray.cmx \
	$(OBJDIR)/tcslist.cmx \
	$(OBJDIR)/tcsset.cmx \
	$(OBJDIR)/tcshash.cmx \
	$(OBJDIR)/tcsqueue.cmx \
	$(OBJDIR)/tcsstrings.cmx \
	$(OBJDIR)/tcsgraph.cmx

MODULES_UTILS=$(OBJDIR)/tcstiming.cmx \
	$(OBJDIR)/tcsstats.cmx \
	$(OBJDIR)/tcsargs.cmx \
	$(OBJDIR)/tcsmessage.cmx

MODULES_AUTOMATA=$(OBJDIR)/tcsautomata.cmx \
	$(OBJDIR)/tcspiterman.cmx \
	$(OBJDIR)/tcsgoodforgamesauto.cmx \
	$(OBJDIR)/tcsmiyanohayashi.cmx \
	$(OBJDIR)/tcsparitygameparser.cmx \
	$(OBJDIR)/tcsparitysolutionparser.cmx \
	$(OBJDIR)/tcsltsparser.cmx \
	$(OBJDIR)/tcstsparser.cmx \
	$(OBJDIR)/tcsautomataparser.cmx

MODULES_INTF=$(MODULES_DATA) \
	$(MODULES_UTILS) \
	$(MODULES_AUTOMATA)

PREMODULES=$(OBJDIR)/tcsautomataparserinternal.cmx $(OBJDIR)/tcsparitygamelexer.cmx $(OBJDIR)/tcsparitysolutionlexer.cmx $(OBJDIR)/tcsltslexer.cmx $(OBJDIR)/tcstslexer.cmx

PREINTF=$(OBJDIR)/tcsautomataparserinternal.cmi

MODULES=$(PREMODULES) $(MODULES_INTF)

INTERFACES=$(MODULES_INTF:.cmx=.cmi)

all: modules library tests

modules: $(INTERFACES) $(PREINTF) $(PREMODULES) $(MODULES)

LIBRARYNAME=$(OBJDIR)/tcslib.cmxa

library: modules libexec

libexec:
	$(OCAMLOPT) -a -o $(LIBRARYNAME) $(CPPCOMPILER) $(MODULES)

tests: modules graphtest1 isettest1 isettest2 argstest

graphtest1: $(OBJDIR)/graphtest1.cmx
	$(OCAMLOPT) $(CPPCOMPILER) nums.cmxa -o $(BINDIR)/graphtest1 $(MODULES) $(OBJDIR)/graphtest1.cmx

isettest1: $(OBJDIR)/isettest1.cmx
	$(OCAMLOPT) $(CPPCOMPILER) nums.cmxa bigarray.cmxa -o $(BINDIR)/isettest1 $(MODULES) $(OBJDIR)/isettest1.cmx

isettest2: $(OBJDIR)/isettest2.cmx
	$(OCAMLOPT) $(CPPCOMPILER) nums.cmxa bigarray.cmxa -o $(BINDIR)/isettest2 $(MODULES) $(OBJDIR)/isettest2.cmx

argstest: $(OBJDIR)/argstest.cmx
	$(OCAMLOPT) $(CPPCOMPILER) nums.cmxa bigarray.cmxa -o $(BINDIR)/argstest $(MODULES) $(OBJDIR)/argstest.cmx

$(OBJDIR)/%.cmx: $(SRCDIR)/tests/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/data/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/data/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/automata/parser/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/automata/parser/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/utils/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/utils/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(SRCDIR)/automata/parser/tcsparitygameparser.mli: $(SRCDIR)/automata/parser/tcsparitygameparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitygameparser.mly

$(SRCDIR)/automata/parser/tcsparitygameparser.ml: $(SRCDIR)/automata/parser/tcsparitygameparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitygameparser.mly

$(SRCDIR)/automata/parser/tcsparitygamelexer.ml: $(SRCDIR)/automata/parser/tcsparitygamelexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsparitygamelexer.mll

$(SRCDIR)/automata/parser/tcsparitysolutionparser.mli: $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly

$(SRCDIR)/automata/parser/tcsparitysolutionparser.ml: $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsparitysolutionparser.mly

$(SRCDIR)/automata/parser/tcsparitysolutionlexer.ml: $(SRCDIR)/automata/parser/tcsparitysolutionlexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsparitysolutionlexer.mll

$(SRCDIR)/automata/parser/tcsltsparser.mli: $(SRCDIR)/automata/parser/tcsltsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsltsparser.mly

$(SRCDIR)/automata/parser/tcsltsparser.ml: $(SRCDIR)/automata/parser/tcsltsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcsltsparser.mly

$(SRCDIR)/automata/parser/tcsltslexer.ml: $(SRCDIR)/automata/parser/tcsltslexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcsltslexer.mll

$(SRCDIR)/automata/parser/tcstsparser.mli: $(SRCDIR)/automata/parser/tcstsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcstsparser.mly

$(SRCDIR)/automata/parser/tcstsparser.ml: $(SRCDIR)/automata/parser/tcstsparser.mly
	$(OCAMLYACC) $(SRCDIR)/automata/parser/tcstsparser.mly

$(SRCDIR)/automata/parser/tcstslexer.ml: $(SRCDIR)/automata/parser/tcstslexer.mll
	$(OCAMLLEX) $(SRCDIR)/automata/parser/tcstslexer.mll

clean:
	rm -f $(OBJDIR)/*.o \
	      $(OBJDIR)/*.cmx \
		  $(OBJDIR)/*.cmi \
		  $(BINDIR)/graphtest1 \
		  $(BINDIR)/isettest1 \
		  $(BINDIR)/isettest2 \
		  $(BINDIR)/argstest \
		  $(SRCDIR)/automata/parser/tcsparitygameparser.ml \
		  $(SRCDIR)/automata/parser/tcsparitygameparser.mli \
		  $(SRCDIR)/automata/parser/tcsparitygamelexer.ml \
		  $(SRCDIR)/automata/parser/tcsparitysolutionparser.ml \
		  $(SRCDIR)/automata/parser/tcsparitysolutionparser.mli \
		  $(SRCDIR)/automata/parser/tcsparitysolutionlexer.ml \
		  $(SRCDIR)/automata/parser/tcsltsparser.ml \
		  $(SRCDIR)/automata/parser/tcsltsparser.mli \
		  $(SRCDIR)/automata/parser/tcsltslexer.ml \
		  $(SRCDIR)/automata/parser/tcstsparser.ml \
		  $(SRCDIR)/automata/parser/tcstsparser.mli \
		  $(SRCDIR)/automata/parser/tcstslexer.ml
