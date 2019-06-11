######################################################################
# yaws config example
# (C) 2006 Tail-f Systems
#
# See the README file for more information
######################################################################

include ../../include.mk


# overwrite
ERL_OBJECTS = $(ERL_SOURCES:%.erl=%.beam) 
ERL_FLAGS += -I/usr/local/lib/yaws/include

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS)   $<

# Example configuration database directory (default directory in confd.conf)
CDB_DIR     = ./confd-cdb 
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd


all:	yaws.fxs  $(ERL_OBJECTS) $(CDB_DIR)

%.fxs: %.yang
	$(CONFDC) -c $<

######################################################################
clean:
	-rm -rf *.o *.a *.fxs *.xsd $(ERL_OBJECTS) \
	 confd_candidate.db *.log global.data \
		confd.log $(CDB_DIR) running.* etc 2> /dev/null

######################################################################
$(CDB_DIR):
	mkdir -p $(CDB_DIR)
	cp ../conf/aaa_init.xml $(CDB_DIR)
	cp yaws_init.xml $(CDB_DIR)

start:  stop 
	$(CONFD) -c ./confd.conf $(CONFD_FLAGS)

estart: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin -pa /usr/local/lib/yaws/ebin -s yconf

stop:
	$(CONFD) --stop || true


