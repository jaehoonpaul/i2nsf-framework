
CONFD_DIR ?= /usr/local

LIBCONFD = $(CONFD_DIR)/lib/libconfd.a

CFLAGS   += -Wall -g -I$(CONFD_DIR)/include

ifeq ($(shell uname -s),QNX)
LDFLAGS    = -lsocket
else
LDFLAGS    = -lpthread
endif

ifeq ($(shell uname -s),SunOS)
CC         = gcc
LDFLAGS    = -lpthread -lsocket -lnsl
endif

mmap_schema: mmap_schema.c
	$(CC) -o $@ $< $(CFLAGS) $(LIBCONFD) $(LDFLAGS) -lm
