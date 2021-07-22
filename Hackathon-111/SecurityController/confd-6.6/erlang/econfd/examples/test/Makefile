######################################################################
# Test cases and  examples
# (C) 2006 Tail-f Systems
#


# to run this,
# make all && make start
# make destart


include ../../include.mk

CONF ?= confd.conf

# overwrite
ERL_OBJECTS = $(filter-out ebin/starter.beam, $(ERL_SOURCES:%.erl=ebin/%.beam))

FXS = types.fxs procs.fxs smp.fxs action.fxs cdbwrite.fxs simple.fxs \
	ytest.fxs iid.fxs notif.fxs meta-data.fxs

# Example configuration database directory (default directory in confd.conf)
CDB_DIR     = ./confd-cdb
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd


all:	$(FXS) $(ERL_OBJECTS) $(CDB_DIR)

ebin/test.beam: smp.hrl cdbwrite.hrl simple.hrl types.hrl iid.hrl notif.hrl
ebin/simple.beam: simple.hrl
ebin/iid.beam: iid.hrl

ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin  $<

%.fxs: %.yang
	$(CONFDC) -c $<

%.hrl: %.fxs
	$(CONFDC) --emit-hrl $@  $<

%.fxs: %.yang
	$(CONFDC) -c $<




######################################################################
clean: stop
	-rm -rf *.o *.a *.fxs *.hrl *.xsd $(ERL_OBJECTS) *.beam ebin/*.beam \
	 confd_candidate.db *.log *.out confderr.log.* global.data \
		smp.hrl cdbwrite.hrl simple.hrl netconf.trace \
		confd.log $(CDB_DIR) running.* etc

######################################################################
$(CDB_DIR):
	mkdir -p $(CDB_DIR)
	sed 's/group>admin/group>*/' ../conf/aaa_init.xml > $(CDB_DIR)/aaa_init.xml
	cp action_init.xml $(CDB_DIR)

## start confd daemon
start:  stop
	$(CONFD) --start-phase0 -c $(CONF) $(CONFD_FLAGS)

## start erlang
estart: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin ebin -noinput -s test

## start erlang, run tests and don't halt
destart: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin ebin -s test dstart


## debug target, start and setup erlang daemon, don't run any tests
setup: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin ebin -s test setup


## test EOF handling when starting confd via open_port/2
pstart:
	CONFD=$(CONFD) CONFD_FLAGS="$(CONFD_FLAGS)" \
		$(ERL) -pa ../../ebin ebin -noinput -s test pstart
	rm -f $(CDB_DIR)/*.cdb

## test using econfd internally in ConfD
## - the test is started by load of ebin/starter.beam
istart: ebin/starter.beam stop
	$(CONFD) --start-phase0 -c $(CONF) $(CONFD_FLAGS) \
		--addloadpath ebin --foreground

stop:
	$(CONFD) --stop > /dev/null 2>&1 || true
