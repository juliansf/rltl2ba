RLTL2BA := ../../rltl2ba
SOURCES := $(wildcard *.rltl)
NBW = $(SOURCES:.rltl=.nbw.dot)

all: $(NBW)

$SUFFIXES: .rltl .dot

%.nbw.dot: %.rltl
	$(RLTL2BA) -dot -o $@ -nbw $<

.PHONY: clean

clean:
	rm -f *.dot *~
