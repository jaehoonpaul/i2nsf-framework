######################################################################
# (C) 2014 Tail-f Systems
######################################################################

CONFDC    = $(CONFD_DIR)/bin/confdc
SMIDUMP  ?= $(CONFD_DIR)/bin/smidump

INCLUDE   = -I$(CONFD_DIR)/include -I..
ifdef ADD_CUSTOM_INCLUDE_DIR
INCLUDE   += -I$(ADD_CUSTOM_INCLUDE_DIR)
endif

# Alternative setting of the external commands used by linuxcfg
#USER_MODE_CMDS += -D 'USER_IP_CMD="/sbin/ip"'
#USER_MODE_CMDS += -D 'USER_VCONFIG_CMD="/sbin/vconfig"'
#USER_MODE_CMDS += -D 'USER_ETHTOOL_CMD="/sbin/ethtool"'
#USER_MODE_CMDS += -D 'USER_DHCPD_CMD="/usr/sbin/dhcpd"'
#USER_MODE_CMDS += -D 'USER_NTPD_CMD="/usr/sbin/ntpd"'
#USER_MODE_CMDS += -D 'USER_NTPQ_CMD="/usr/bin/ntpq"'

# Dynamic interface handling
ifdef DYN_IFACE
CUSTOM_MODULES += -DDYN_IFACE
DYN_IF_CHECK_INT ?= 10
CUSTOM_PARAMS += -DDYN_IF_CHECK_INT=$(DYN_IF_CHECK_INT)
endif

CWARN     = -Wall -Werror

CFLAGS	  = $(CWARN) -g \
	-DNS_PREFIX=$(NS_PREFIX) $(INCLUDE) \
	$(CUSTOM_MODULES) $(CUSTOM_PARAMS) \
	$(USER_MODE_CMDS) $(CFLAGS_LINUXCFG_FEATURES)

%.h: %.fxs
	$(CONFDC) --emit-h $@ $<
