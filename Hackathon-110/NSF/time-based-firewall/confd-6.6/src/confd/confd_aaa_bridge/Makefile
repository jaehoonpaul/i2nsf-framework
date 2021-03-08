## Set CONFD_DIR to the top of the ConfD installation


CONFD		 = $(CONFD_DIR)/bin/confd
CONFDC		 = $(CONFD_DIR)/bin/confdc
INCLUDE		 = -I$(CONFD_DIR)/include
CONFD_LIB	 ?= $(CONFD_DIR)/lib/libconfd.a
CC		 = gcc
OBJS		 = confd_aaa_bridge.o

LIBS		 = $(CONFD_LIB) -lpthread -lm
CFLAGS		 = -Wall -g $(INCLUDE)


all: aaa_bridge@2011-09-22.fxs aaa_bridge.h confd_aaa_bridge

aaa_bridge@2011-09-22.fxs: ../aaa/tailf-aaa@2011-09-22.yang bridge-ann.yang
	$(CONFDC) -c -a bridge-ann.yang -o $@ -- $<

confd_aaa_bridge: $(OBJS)
	$(CC) -o $@ $(OBJS)  $(LIBS)

%.o:    %.c aaa_bridge.h
	$(CC) $(CFLAGS) -c $<

aaa_bridge.h: aaa_bridge@2011-09-22.fxs
	$(CONFDC) --emit-h $@ $<

clean:
	-rm aaa_bridge.h aaa_bridge@2011-09-22.fxs \
	    confd_aaa_bridge confd_aaa_bridge.o

