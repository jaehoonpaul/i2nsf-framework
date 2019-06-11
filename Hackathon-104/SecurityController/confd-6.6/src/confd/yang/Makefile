CONFDC=$(CONFD_DIR)/bin/confdc

YANG_SOURCES        = $(wildcard *.yang)
SUBMODULES          = $(shell grep -l belongs-to *.yang)
ANNOTATION_MODULES  = $(wildcard *-ann.yang)

BUILTIN_MODULES     += ietf-netconf.yang
BUILTIN_MODULES     += ietf-netconf-with-defaults.yang
BUILTIN_MODULES     += tailf-netconf-inactive.yang
BUILTIN_MODULES     += tailf-netconf-transactions.yang
BUILTIN_MODULES     += tailf-netconf-query.yang
BUILTIN_MODULES     += tailf-rest-query.yang
BUILTIN_MODULES     += tailf-rest-error.yang
BUILTIN_MODULES     += tailf-netconf-extensions.yang

YANG_MODULES = $(filter-out $(SUBMODULES) $(ANNOTATION_MODULES) \
                   $(BUILTIN_MODULES),$(YANG_SOURCES))

ALL_FXS_FILES = $(YANG_MODULES:%.yang=%.fxs)

%.fxs: %.yang %-ann.yang
	$(CONFDC) -c -o $@ -a $*-ann.yang -- $<

%.fxs: %.yang
	$(CONFDC) -c -o $@ -- $<

all:	$(ALL_FXS_FILES)

clean:
	-rm -f *.fxs 2>/dev/null
