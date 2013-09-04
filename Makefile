# OASIS_START
# DO NOT EDIT (digest: bc1e05bfc8b39b664f29dae8dbd3ebbb)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: 
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: 
	$(SETUP) -clean $(CLEANFLAGS)

distclean: 
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

CUDD_DIR=lib/cudd/cudd-2.4.2
CUDD_BUILD=_build/lib/cudd

cudd: 
	@echo "Making the CUDD Package."
	@cd $(CUDD_DIR); make 
	@mkdir -p $(CUDD_BUILD);
	@cp -rf $(CUDD_DIR)/lib $(CUDD_DIR)/include $(CUDD_BUILD)

cudddistclean:
	@cd $(CUDD_DIR); make distclean
