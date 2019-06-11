
SRC     = ipc_drv_unix.c
#SRC    = ipc_drv_etcp.c

DESTDIR = ../../../lib/confd/lib/core/confd/priv
SHOBJ   = $(DESTDIR)/ipc_drv_ops.so

OBJ     = $(SRC:.c=.o)
CC      = gcc
CFLAGS  = -Wall -O2 -fpic
LD      = gcc
LDFLAGS = -shared

all: $(SHOBJ)

$(SHOBJ): $(OBJ)
	$(LD) $(LDFLAGS) -o $@ $^

clean:
	rm -f $(SHOBJ) $(OBJ)
