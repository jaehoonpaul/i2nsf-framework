######################################################################
# Extern db example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

include ../../include.mk


# overwrite
ERL_OBJECTS = $(ERL_SOURCES:%.erl=%.beam) 

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS)   $<

$(ERL_OBJECTS): smp.hrl

# Example configuration database directory (default directory in confd.conf)
CDB_DIR     = ./confd-cdb 
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd


all:	smp.fxs  smp.hrl $(ERL_OBJECTS) $(CDB_DIR)

%.fxs: %.yang
	$(CONFDC) -c $<

%.hrl: %.fxs
	$(CONFDC) --emit-hrl $@ $<

######################################################################
clean:
	-rm -rf *.o *.a *.xso *.fxs *.xsd $(ERL_OBJECTS) smp.hrl \
	 confd_candidate.db *.log global.data \
		confd.log $(CDB_DIR) running.* etc

######################################################################
$(CDB_DIR):
	mkdir -p $(CDB_DIR)
	cp ../conf/aaa_init.xml $(CDB_DIR)

start:  stop 
	$(CONFD) -c ./confd.conf $(CONFD_FLAGS)

estart: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin -s simple

stop:
	$(CONFD) --stop || true


test:	all
	$(MAKE) start
	$(ERL) -detached -pa ../../ebin -s simple
	sleep 4
	MAKE=$(MAKE) sh -c ./cli_script



