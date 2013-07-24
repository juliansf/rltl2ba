# Adapt the following paths
extlib=./extlib-1.5
cudd=./cudd-2.4.2
src=src
obj=obj
bin=bin


incl = -I $(src) -I $(obj) -I $(extlib) -I $(cudd)
modules = cudd lexer parser psl sets graph nfa smv aba pslaba nba main
cuddlibs = \
	$(cudd)/cudd/libcudd.a \
	$(cudd)/mtr/libmtr.a \
	$(cudd)/st/libst.a \
	$(cudd)/epd/libepd.a \
	$(cudd)/util/libutil.a



all: clean $(bin)/psl2ba test_eq

# ===== Generate bin/psl2ba
$(bin)/psl2ba: $(obj)/lexer.cmo $(obj)/parser.cmo $(extlib)/extLib.cma $(cuddlibs) $(obj)/cudd.cma $(modules:%=$(obj)/%.cmo)
	ocamlc -o $@ -custom $+
	#ocamlc -o ./test/genex $(extlib)/extLib.cma $(modules:=.cmo) ./test/genex.ml

$(obj)/%.cmo: $(src)/%.ml
	ocamlc $(incl) -o $@ -c $<

$(obj)/lexer.cmo $(obj)/parser.cmo:
	ocamllex $(src)/lexer.mll               # generates src/lexer.ml
	ocamlyacc $(src)/parser.mly             # generates src/parser.ml and src/parser.mli
	ocamlc -i $(src)/psl.ml > $(src)/psl.mli
	# compiling
	ocamlc $(incl) -c -o $(obj)/psl.cmi $(src)/psl.mli
	ocamlc $(incl) -c -o $(obj)/psl.cmo $(src)/psl.ml
	ocamlc $(incl) -c -o $(obj)/parser.cmi $(src)/parser.mli
	ocamlc $(incl) -c -o $(obj)/lexer.cmo $(src)/lexer.ml
	ocamlc $(incl) -c -o $(obj)/parser.cmo $(src)/parser.ml


# ===== Generate bin/test_eq
test_eq: $(extlib)/extLib.cma
	ocamlc -c -I $(extlib) test/test_eq.ml
	ocamlc -o test/test_eq $(extlib)/extLib.cma str.cma ./test/test_eq.cmo


# ===== Generate ExtLib library
$(extlib)/extLib.cma:
	make --directory=$(extlib)


# ===== Generate Cudd stubs
$(obj)/cudd.cma: $(src)/cudd.c $(obj)/cudd.cmo
	ocamlc -I $(cudd)/include/ -c -cc "gcc -o $(obj)/cudd.o" $(src)/cudd.c
	ocamlc -a -o $(obj)/cudd.cma -custom $(obj)/cudd.o $(obj)/cudd.cmo

$(cudd)/cudd/libcudd.a:
	@echo ""
	@echo "============================================================"
	@echo "CUDD LIBRARY NOT FOUND:"
	@echo "Please go to the $(cudd) directory, adjust the Makefile"
	@echo "for your operating system, and compile CUDD."
	@echo "============================================================"
	@echo ""

# ===== Generate documentation
doc: all
	mkdir -p doc/
	ocamldoc -sort -html -d doc/ $(incl) $(modules:%=$(src)/%.ml)


clean:
	rm -f obj/*
	rm -f bin/*
	rm -f doc/*
	rm -f psl2ba
	rm -f psl.mli
	rm -f lexer.ml
	rm -f parser.ml[i]
	rm -f *.cm[iox]
	rm -f *.*~
	rm -f *~
	rm -f ./test/genex
	rm -f ./test/*.cm[iox]
	rm -f ./test/*.*~
	rm -f ./test/*~
