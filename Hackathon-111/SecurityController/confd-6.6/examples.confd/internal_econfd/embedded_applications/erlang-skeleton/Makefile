SUBDIRS = $(shell ls erlang-lib/*/Makefile | sed 's:/Makefile::')

all clean:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( cd $$d && $(MAKE) $@ ) || exit 1 ; \
	    fi ; \
	  done
