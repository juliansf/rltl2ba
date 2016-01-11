RLTL2BA := ../../rltl2ba
SOURCES := $(wildcard *.rltl)
AHW = $(SOURCES:.rltl=.ahw.dot)

all: $(AHW)

$SUFFIXES: .rltl .dot

%.ahw.dot: %.rltl
	$(RLTL2BA) -dot -o $@ -ahw $<

.PHONY: clean

clean:
	rm -f *.dot *~
