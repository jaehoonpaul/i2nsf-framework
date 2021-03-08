######################################################################
# Procs example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

include ../../include.mk


# overwrite
ERL_OBJECTS = $(ERL_SOURCES:%.erl=%.beam) 

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS)   $<

$(ERL_OBJECTS): procs.hrl

# Example configuration database directory (default directory in confd.conf)
CDB_DIR     = ./confd-cdb 
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd


all:	procs.fxs  $(ERL_OBJECTS) $(CDB_DIR)

%.fxs: %.yang
	$(CONFDC) -c $<

%.hrl: %.fxs
	$(CONFDC) --emit-hrl $@ $<

######################################################################
clean:
	-rm -rf *.o *.a *.fxs *.xsd $(ERL_OBJECTS) *.hrl \
	 confd_candidate.db *.log global.data \
		confd.log $(CDB_DIR) running.* etc

######################################################################
$(CDB_DIR):
	mkdir -p $(CDB_DIR)
	cp ../conf/aaa_init.xml $(CDB_DIR)

start:  stop 
	$(CONFD) -c ./confd.conf $(CONFD_FLAGS)

estart:
	$(ERL) -pa ../../ebin -s procs

stop:
	$(CONFD) --stop || true


