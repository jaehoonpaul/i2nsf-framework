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

# Example configuration database directory (default directory in confd.conf)
CDB_DIR     = ./confd-cdb 
CONFD_FLAGS = --addloadpath $(CONFD_DIR)/etc/confd


all:	notif.fxs  $(ERL_OBJECTS) $(CDB_DIR)

%.fxs: %.yang
	$(CONFDC) -c $<

######################################################################
clean:
	-rm -rf *.o *.a *.fxs $(ERL_OBJECTS) \
	 confd_candidate.db *.log *.out global.data \
		confd.log $(CDB_DIR) running.* etc 2> /dev/null

######################################################################
$(CDB_DIR):
	mkdir -p $(CDB_DIR)
	cp ../conf/aaa_init.xml $(CDB_DIR)

start:  stop 
	$(CONFD) -c ./confd.conf $(CONFD_FLAGS)

estart: $(ERL_OBJECTS)
	$(ERL) -pa ../../ebin -s notif

stop:
	$(CONFD) --stop || true

test:	all
	$(MAKE) start
	$(ERL) -detached -pa ../../ebin -s notif
	sleep 3
	netconf-console-tcp --get -x netconf > netconf.out
	diff netconf.expect netconf.out
	netconf-console-tcp -s raw sub.xml > sub.out & sleep 8; kill $$!
	grep "<flags>17</flags>" sub.out >/dev/null
	grep "</linkProperty>" sub.out >/dev/null
	grep "</linkUp>" sub.out >/dev/null
	grep "<obj.*ifIndex='[0-9]*[02468]'.*</obj" sub.out >/dev/null
	grep "<refs.*ifIndex='[0-9]*[13579]'.*ifIndex</refs" sub.out >/dev/null
	grep "<refs.*ifIndex='[0-9]*[02468]'.*desc</refs" sub.out >/dev/null
	netconf-console-tcp -s raw replay.xml > replay.out & sleep 4; kill $$!
	grep replayComplete replay.out
	$(ERL) -pa ../../ebin -noshell \
		-s subscriber start live false 2 > sub.out
	grep 'linkUp.*start' sub.out
	grep 'linkDown.*start' sub.out
	$(ERL) -pa ../../ebin -noshell \
		-s subscriber start live true 2 > sub.out
	grep 'linkUp.*start' sub.out
	if grep 'linkDown.*start' sub.out; then false; fi
	$(ERL) -pa ../../ebin -noshell \
		-s subscriber start replay false undefined > replay.out
	if grep 'linkUp.*start' replay.out; then false; fi
	grep 'linkDown.*start' replay.out
	grep replay_complete replay.out
	$(ERL) -pa ../../ebin -noshell \
		-s subscriber start replay true undefined > replay.out
	if grep 'linkUp.*start' replay.out; then false; fi
	if grep 'linkDown.*start' replay.out; then false; fi
	grep replay_complete replay.out
	$(MAKE) stop; 





