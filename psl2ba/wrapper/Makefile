extlib=../extlib-1.5

all: clean compile

compile: $(extlib)/extLib.cmxa
	ocamlopt -c -I $(extlib) main.ml
	ocamlopt -o _wrapper $(extlib)/extLib.cmxa main.cmx

$(extlib)/extLib.cmxa:
	make opt --directory=$(extlib)

clean:
	rm -f *.exe
	rm -f *.cm[iox]
	rm -f *.*~
	rm -f *~
	