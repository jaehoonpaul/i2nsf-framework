
# Uncomment or pass on command line to use a connection
# to the IPC port instead of the NETCONF TCP transport
#USE_IPC = yes

# Uncomment/adjust or pass on command line to support IPC Access Check
# (when $(USE_IPC) = yes) without the need for an environment variable
#IPC_ACCESS_FILE = /path/to/access/file

SRC := $(wildcard *.c)
OBJS := $(SRC:%.c=%.o)

ifeq ($(USE_IPC), yes)
CFLAGS += -DUSE_IPC
ifneq ($(IPC_ACCESS_FILE),)
CFLAGS += -DIPC_ACCESS_FILE='"$(IPC_ACCESS_FILE)"'
endif
endif

all: netconf-subsys

netconf-subsys: $(OBJS)
	$(CC) -o $@ $(OBJS)

clean:
	rm -f $(OBJS) netconf-subsys
