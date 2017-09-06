###########################################################
#
# INCLUDING CONFIGURATION FILE
#
###########################################################

ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif

SATSOLVERSOBJ=$(SATSOLVERSROOT)/obj

TCSLIBOBJ=$(TCSLIBROOT)/obj


ifeq "$(HASSMT)" "YES"

SMTMODULES=-ccopt "-I$(Z3DIR)/ocaml -L$(Z3DIR)/ocaml -L$(Z3DIR)/lib" -cclib -lz3 $(OCAML_DIR)/libcamlidl.a $(Z3DIR)/ocaml/z3.cmxa

endif


###########################################################
#
# SETUP INCLUDES DIRECTIVE, SATSOLVERS AND COMPILER
#
###########################################################

INCLUDES=-I $(SRCDIR) -I $(OBJDIR) -I $(OCAML_DIR) -I $(SATSOLVERSOBJ) -I $(TCSLIBOBJ) -I $(Z3DIR)/ocaml

include $(SATSOLVERSROOT)/Config.include

# ifeq "$(HASSAT)" "YES"

CPPCOMPILER=-cc $(OCAMLOPTCPP)

# endif


###########################################################
#
# INCLUDE LIST OF SOLVERS
#
###########################################################

PGSOLVERSOBJ=$(OBJDIR)

include SolverList



###########################################################
#
# INCLUDE LIST OF GENERATORS
#
###########################################################

GENERATORSOBJ=$(OBJDIR)

include GeneratorList

ifeq "$(LINKGENERATORS)" "YES"

GENERATOR_MODULES=$(GENERATORS)

endif

SATSOLVERMODSX=$(SATSOLVERSOBJ)/satwrapper.cmx \
        $(SATSOLVERSOBJ)/satsolvers.cmx



MODULES_WITHOUT_SOLVERS_AND_LIB=$(OBJDIR)/basics.cmx \
        $(OBJDIR)/info.cmx \
	$(OBJDIR)/whoiswho.cmx \
	$(OBJDIR)/graph.cmx \
	$(OBJDIR)/paritygame.cmx \
	$(OBJDIR)/transformations.cmx \
	$(OBJDIR)/specialsolve.cmx \
	$(OBJDIR)/univsolve.cmx \
	$(OBJDIR)/verification.cmx \
	$(OBJDIR)/solvers.cmx \
	$(OBJDIR)/generators.cmx

MODULES_WITHOUT_SOLVERS = $(TCSLIBOBJ)/tcslib.cmxa \
	$(MODULES_WITHOUT_SOLVERS_AND_LIB)

MODULES=$(MODULES_WITHOUT_SOLVERS) \
	$(PGSOLVERS)

INTERFACES=$(MODULES:.cmx=.cmi)

EXECUTABLE=$(BINDIR)/pgsolver
LIBRARYNAME=$(OBJDIR)/libpgsolver.cmxa

PACKAGE=pgsolver.tar

pgsolver: satsolvers $(INTERFACES) $(MODULES) $(GENERATOR_MODULES) main exec

library: satsolvers $(INTERFACES) $(OBJDIR)/libpgsolver.cmi $(MODULES) $(OBJDIR)/libpgsolver.cmx libexec

all: pgsolver library generators tools

satsolvers: $(SATSOLVERSOBJ)/satsolvers.cmx

$(TCSLIBOBJ)/%.cmi:
	make -C $(TCSLIBROOT) all

$(TCSLIBOBJ)/%.cmxa:
	make -C $(TCSLIBROOT) all

$(SATSOLVERSOBJ)/satsolvers.cmx:
	make -C $(SATSOLVERSROOT) all

main: $(OBJDIR)/main.cmx

exec:
	$(OCAMLOPT) -o $(EXECUTABLE) $(CPPCOMPILER) nums.cmxa $(SMTMODULES) $(SATSOLVERMODSX) $(SATSOLVERMODS) $(MODULES) $(GENERATOR_MODULES) $(OBJDIR)/main.cmx

libexec:
	$(OCAMLOPT) -a -o $(LIBRARYNAME) $(CPPCOMPILER) $(MODULES_WITHOUT_SOLVERS_AND_LIB) $(PGSOLVERS) $(OBJDIR)/libpgsolver.cmx

$(OBJDIR)/%.cmx: $(SRCDIR)/pgsolver/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/generators/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/generators/stratimpr/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/tools/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/paritygame/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/solvers/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/utils/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmx: $(SRCDIR)/%.ml
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/paritygame/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/solvers/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/utils/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/pgsolver/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/%.mli
	$(OCAMLOPT) $(INCLUDES) -c -o $@ $<

whoiswho: $(SRCDIR)/pgsolver/whoiswho.ml

$(SRCDIR)/pgsolver/whoiswho.ml: $(SRCDIR)/pgsolver/encipher.ml
	$(OCAML) $< > $@

generators: $(GENERATORS:$(OBJDIR)/%.cmx=%)

tools: obfuscator transformer compressor combine benchmark infotool

%: $(SRCDIR)/generators/%.ml $(OBJDIR)/%.cmx $(OBJDIR)/rungenerator.cmx
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/$@ nums.cmxa $(OBJDIR)/info.cmx $(TCSLIBOBJ)/tcslib.cmxa $(OBJDIR)/graph.cmx $(OBJDIR)/paritygame.cmx $(OBJDIR)/generators.cmx $(OBJDIR)/$@.cmx $(OBJDIR)/rungenerator.cmx

%: $(SRCDIR)/generators/stratimpr/%.ml $(OBJDIR)/%.cmx $(OBJDIR)/rungenerator.cmx
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/$@ nums.cmxa $(OBJDIR)/info.cmx $(TCSLIBOBJ)/tcslib.cmxa $(OBJDIR)/graph.cmx $(OBJDIR)/paritygame.cmx $(OBJDIR)/generators.cmx $(OBJDIR)/$@.cmx $(OBJDIR)/rungenerator.cmx

OBFUSCATOR_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
                   $(OBJDIR)/basics.cmx \
                   $(OBJDIR)/info.cmx \
				   $(OBJDIR)/graph.cmx \
                   $(OBJDIR)/paritygame.cmx \
                   $(OBJDIR)/obfuscator.cmx

obfuscator: $(INTERFACES) $(OBFUSCATOR_MODULES)
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/obfuscator $(OBFUSCATOR_MODULES)

COMBINE_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
                   $(OBJDIR)/basics.cmx \
                   $(OBJDIR)/info.cmx \
                   $(OBJDIR)/graph.cmx \
                   $(OBJDIR)/paritygame.cmx \
                   $(OBJDIR)/transformations.cmx \
                   $(OBJDIR)/combine.cmx \

combine: $(INTERFACES) $(COMBINE_MODULES)
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/combine $(COMBINE_MODULES)

INFOTOOL_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
                   $(OBJDIR)/basics.cmx \
                   $(OBJDIR)/info.cmx \
                   $(OBJDIR)/graph.cmx \
                   $(OBJDIR)/paritygame.cmx \
                   $(OBJDIR)/transformations.cmx \
                   $(OBJDIR)/infotool.cmx \

infotool: $(INTERFACES) $(INFOTOOL_MODULES)
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/infotool $(INFOTOOL_MODULES)

IMPRARENA_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
                   $(OBJDIR)/basics.cmx \
                   $(OBJDIR)/info.cmx \
                   $(OBJDIR)/graph.cmx \
                   $(OBJDIR)/paritygame.cmx \
                   $(OBJDIR)/solvers.cmx \
                   $(OBJDIR)/specialsolve.cmx \
                   $(OBJDIR)/transformations.cmx \
                   $(OBJDIR)/univsolve.cmx \
                   $(OBJDIR)/stratimpralgs.cmx \
                   $(OBJDIR)/imprarena.cmx \

imprarena: $(INTERFACES) $(IMPRARENA_MODULES)
	$(OCAMLOPT) $(CPPCOMPILER) str.cmxa -o $(BINDIR)/imprarena $(IMPRARENA_MODULES)

TRANSFORMER_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
                   $(OBJDIR)/basics.cmx \
                   $(OBJDIR)/info.cmx \
                   $(OBJDIR)/graph.cmx \
                   $(OBJDIR)/paritygame.cmx \
                   $(OBJDIR)/transformations.cmx \
                   $(OBJDIR)/transformer.cmx

transformer: $(INTERFACES) $(TRANSFORMER_MODULES)
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/transformer $(TRANSFORMER_MODULES)

ITERSAT_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
        $(OBJDIR)/basics.cmx \
		$(OBJDIR)/info.cmx \
		$(OBJDIR)/graph.cmx \
		$(OBJDIR)/paritygame.cmx \
		$(SATSOLVERSOBJ)/satwrapper.cmx \
		$(SATSOLVERSOBJ)/satsolvers.cmx \
		$(SATSOLVERSOBJ)/pseudosatwrapper.cmx \
		$(SATSOLVERSOBJ)/preprocessor.cmx \
		$(SATSOLVERSOBJ)/externalsat.cmx \
		$(SATSOLVERMODS)

ITERSAT_INTERFACES=$(ITERSAT_MODULES:.cmx=.cmi)

itersat: satsolvers $(ITERSAT_INTERFACES) $(ITERSAT_MODULES) $(OBJDIR)/itersat.cmx
	$(OCAMLOPT) -o $(BINDIR)/itersat $(CPPCOMPILER) $(ITERSAT_MODULES) $(OBJDIR)/itersat.cmx

benchmark: satsolvers $(INTERFACES) $(MODULES) $(OBJDIR)/benchmark.cmx
	$(OCAMLOPT) $(LPMODULESCC) -o $(BINDIR)/benchmark $(CPPCOMPILER) nums.cmxa $(SMTMODULES) $(SATSOLVERMODSX) $(SATSOLVERMODS) $(MODULES) $(OBJDIR)/benchmark.cmx

benchstratimpr: $(INTERFACES) $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.cmi $(OBJDIR)/stratimpralgs.cmx $(OBJDIR)/benchstratimpr.cmx
	$(OCAMLOPT) -o $(BINDIR)/benchstratimpr $(CPPCOMPILER) nums.cmxa $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.cmx $(OBJDIR)/benchstratimpr.cmx

policyitervis: $(INTERFACES) $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.cmi $(OBJDIR)/stratimpralgs.cmx $(OBJDIR)/policyitervis.cmx
	$(OCAMLOPT) -o $(BINDIR)/policyitervis $(CPPCOMPILER) str.cmxa nums.cmxa $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.cmx $(OBJDIR)/policyitervis.cmx

COMPRESSOR_MODULES=$(TCSLIBOBJ)/tcslib.cmxa \
                   $(OBJDIR)/info.cmx \
                   $(OBJDIR)/basics.cmx \
                   $(OBJDIR)/graph.cmx \
                   $(OBJDIR)/paritygame.cmx \
                   $(OBJDIR)/transformations.cmx \
                   $(OBJDIR)/compressor.cmx

compressor: $(INTERFACES) $(COMPRESSOR_MODULES)
	$(OCAMLOPT) $(CPPCOMPILER) -o $(BINDIR)/compressor $(COMPRESSOR_MODULES)


clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.cmx $(OBJDIR)/*.cmi
	rm -f $(SRCDIR)/pgsolver/whoiswho.ml
	rm -f $(EXECUTABLE)

cleansat:
	make -C $(SATSOLVERSROOT) clean

veryclean: clean
	rm -f $(BINDIR)/randomgame $(BINDIR)/clusteredrandomgame $(BINDIR)/steadygame $(BINDIR)/recursiveladder $(BINDIR)/expstratimpr $(BINDIR)/jurdzinskigame $(BINDIR)/laddergame $(BINDIR)/obfuscator $(BINDIR)/transformer $(BINDIR)/compressor $(BINDIR)/benchmark $(BINDIR)/itersat

cleanall: cleansat veryclean

package: whoiswho
	$(MAKE) -C doc
	$(TAR) cvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=$(SRCDIR)/temp/* --exclude=*~ --transform "s,^,pgsolver/," Makefile GeneratorList SolverList Config.default README install.txt changelog.txt src bin obj doc/pgsolver.pdf
	$(TAR) rvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,SATSolversForOCaml,pgsolver/satsolvers," $(SATSOLVERSROOT)/bin $(SATSOLVERSROOT)/obj $(SATSOLVERSROOT)/src $(SATSOLVERSROOT)/tester $(SATSOLVERSROOT)/Makefile $(SATSOLVERSROOT)/Config.default $(SATSOLVERSROOT)/Config.include $(SATSOLVERSROOT)/Solvers.default
	$(TAR) rvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,TCSlib,pgsolver/TCSlib," $(TCSLIBROOT)/bin $(TCSLIBROOT)/obj $(TCSLIBROOT)/src $(TCSLIBROOT)/Makefile $(TCSLIBROOT)/Config.default
	$(GZIP) $(PACKAGE)
#	$(TAR) czvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=satsolvers/$(OBJDIR)/* --exclude=satsolvers/$(BINDIR)/* --exclude=$(SRCDIR)/temp/* --exclude=*~ --exclude="$(SRCDIR)/pgsolver/encipher.*" --transform "s,^,pgsolver/," Makefile Config.default README install.txt src bin obj satsolvers/bin satsolvers/obj satsolvers/src satsolvers/tester satsolvers/Makefile satsolvers/Config.default doc/pgsolver.pdf

DEPENDENCY_INCLUDES=-I $(SRCDIR)/pgsolver \
                    -I $(SRCDIR)/generators \
                    -I $(SRCDIR)/tools \
                    -I $(SRCDIR)/paritygame \
                    -I $(SRCDIR)/solvers \
                    -I $(SRCDIR)/utils \
                    -I $(SRCDIR)

depend:
	$(OCAMLDEP) $(DEPENDENCY_INCLUDES) $(SRCDIR)/*/*ml $(SRCDIR)/*/*.mli | sed "s#$(SRCDIR)/[^/]*#$(OBJDIR)#g" > .depend

ifeq ($(strip $(wildcard .depend)),)
	include .depend
endif
