include ./include.mk
include ./confdvsn.mk

DIRS = src
EDIRS = examples/procs examples/simple_notrans examples/test # examples/yaws
all: app examp

TAR ?= tar

app:
	set -e ; \
	for d in $(DIRS) ; do	\
		( cd $$d &&  $(MAKE)) || exit 1; \
	done

examp:
	set -e; \
	for d in $(EDIRS) ; do \
		(cd $$d && $(MAKE) all) || exit 1; \
	done

clean:
	@set -e ;						\
	for d in $(DIRS) $(EDIRS) ;  do				\
		( cd $$d &&  $(MAKE) clean) || exit 1;	\
	done

DIST_FILE = confd-$(CONFDVSN).econfd.tar

dist:
	$(MAKE) clean
	cd .. && $(TAR) c -h --exclude '*.svn' --exclude '*~' --exclude .gitignore --exclude Makefile.release -f $(DIST_FILE) econfd && gzip -9 $(DIST_FILE)
