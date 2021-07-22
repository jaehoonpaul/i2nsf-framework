######################################################################
# Action example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

include ../../include.mk


# overwrite
ERL_OBJECTS = $(ERL_SOURCES:%.erl=%.beam) 

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS)   $<

# Example configuration database directory (default directory in confd.conf)
CDB_DIR     = ./confd-cdb 
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd


all:	$(ERL_OBJECTS) $(CDB_DIR)

######################################################################
clean:
	-rm -rf *.o *.a *.xso *.fxs *.xsd $(ERL_OBJECTS) \
	 confd_candidate.db *.log \
		confd.log $(CDB_DIR) running.* etc 2> /dev/null

######################################################################
$(CDB_DIR):
	mkdir -p $(CDB_DIR)
	cp ../conf/aaa_init.xml $(CDB_DIR)

start:  stop 
	$(CONFD) -c ./confd.conf $(CONFD_FLAGS)

estart: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin -s math_eapi

stop:
	$(CONFD) --stop || true


