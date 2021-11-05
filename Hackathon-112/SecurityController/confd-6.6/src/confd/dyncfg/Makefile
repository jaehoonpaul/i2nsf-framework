
# If ConfD was installed somewhere else than /usr/local
# set CONFD_DIR here to get include files things right
# or set environment variable to where ConfD got installed

CONFD_DIR  ?= /usr/local
CONFDC      = $(CONFD_DIR)/bin/confdc

all: confd_dyncfg.fxs

confd_dyncfg.fxs: confd_dyncfg.yang
	$(CONFDC) -c -o $@ $<

clean:
	rm -f confd_dyncfg.fxs

