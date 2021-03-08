######################################################################
# (C) 2006 Tail-f Systems
#
######################################################################

# Uncomment or pass on command line to build with external IPC support
#SUPPORT_EXTERNAL_IPC = yes

# Uncomment or pass on command line if your OS does not have getopt_long(3)
#NEED_GETOPT_LONG = yes

CONFD_DIR  = ../../..

STRIP      = strip

SRC        = clistart.c confd_ipc_access.c
OBJS       = $(SRC:.c=.o)
MAAPI_SRC  = maapi.c
MAAPI_OBJS = $(MAAPI_SRC:.c=.o)

CFLAGS    += -Wall -I../ipc_drv -I$(CONFD_DIR)/include

ifeq ($(NEED_GETOPT_LONG), yes)
SRC       += getopt_long.c
MAAPI_SRC += getopt_long.c
CFLAGS    += -I.
endif

ifeq ($(SUPPORT_EXTERNAL_IPC), yes)
CFLAGS    += -DEXTERNAL_IPC
ifeq ($(shell uname -s),Linux)
LDFLAGS    = -ldl
endif
endif

ifeq ($(shell uname -s),QNX)
MAAPI_LDFLAGS = -lsocket
else
MAAPI_LDFLAGS = -lpthread
endif

ifeq ($(shell uname -s),SunOS)
CC            = gcc
MAAPI_LDFLAGS = -lpthread -lsocket -lnsl
endif


all: confd_cli maapi

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

confd_cli: $(OBJS)
	$(CC) -o $@ $^ $(LDFLAGS)
	$(STRIP) $@

maapi: $(MAAPI_OBJS)
	$(CC) -o $@ $^ -L$(CONFD_DIR)/lib -lconfd $(MAAPI_LDFLAGS) $(LDFLAGS)
	$(STRIP) $@

clean:
	rm -f $(OBJS) $(MAAPI_OBJS) confd_cli maapi


