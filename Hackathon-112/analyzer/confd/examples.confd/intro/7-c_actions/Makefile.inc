# Make sure makefiles which have a usage target first use it even if
# they include this file at the top...

iusage: usage

# Define common ConfD build tools and flags

OSNAME		 := $(shell uname -s)

CONFD		 = $(CONFD_DIR)/bin/confd
CONFDC		 = $(CONFD_DIR)/bin/confdc
INCLUDE		 = -I$(CONFD_DIR)/include
CONFD_LIB	?= $(CONFD_DIR)/lib/libconfd.a
ifeq ($(OSNAME),QNX)
LIBS		 = $(CONFD_LIB) -lsocket -lm
else
LIBS		 = $(CONFD_LIB) -lpthread -lm
endif
CFLAGS		 = -Wall -g $(INCLUDE)
CDB_DIR		 = ./confd-cdb

ifeq ($(OSNAME),QNX)
KILLALL		 = slay -f
else
ifeq ($(OSNAME),NetBSD)
KILLALL		 = pkill -x
else
ifeq ($(OSNAME),SunOS)
KILLALL		 = pkill -x
else
KILLALL		 = killall
endif
endif
endif

ifeq ($(OSNAME),FreeBSD)
CFLAGS		+= -I/usr/local/include
LIBS		+= -L/usr/local/lib
endif
ifeq ($(OSNAME),NetBSD)
CFLAGS		+= -I/usr/pkg/include
LIBS		+= -L/usr/pkg/lib
endif
ifeq ($(OSNAME),QNX)
CFLAGS		+= -D_POSIX_C_SOURCE=200112 -I/opt/include -I/usr/local/include -I/usr/pkg/include
LIBS		+= -L/opt/lib -L/usr/local/lib -L/usr/pkg/lib
endif
ifeq ($(OSNAME),SunOS)
SHELL		= /usr/xpg4/bin/sh
MAKEARGS	= SHELL=$(SHELL)
export PATH	:= /opt/csw/bin:/usr/local/bin:/usr/xpg4/bin:$(PATH)
CC		= gcc
LIBS		+= -lsocket -lnsl
endif

ifeq ($(OSNAME),Darwin)
SHARED_FLAGS	= -dynamiclib
LD_ENV		= DYLD_LIBRARY_PATH
else
SHARED_FLAGS	= -shared
LD_ENV		= LD_LIBRARY_PATH
endif
# Targets to require/reject specific OS

.PHONY: linux not_sunos

linux:
ifneq ($(OSNAME),Linux)
	@echo "This example only works on Linux" ; exit 1
endif

not_sunos:
ifeq ($(OSNAME),SunOS)
	@echo "This example does not work on Solaris" ; exit 1
endif

iclean:
	-rm -rf *.o *.a *.xso *.fxs *.xsd *.log *.db *.ccl \
               host.key host.cert client.key client.cert ca.cert *_proto.h \
               running.invalid global.data aaa_cdb.* $(CDB_DIR) ssh-keydir \
               webui etc
