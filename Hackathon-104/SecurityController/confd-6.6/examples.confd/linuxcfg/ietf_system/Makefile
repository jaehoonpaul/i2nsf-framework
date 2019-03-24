######################################################################
# (C) 2014 Tail-f Systems
######################################################################

COMPONENT      = ietf_system
COMPONENT_SRCS = ietf_system.c tz.c ntp.c dns.c

FXS = ietf-system.fxs

CONFDC_FEATURES_ietf-system=ietf-system:ntp,timezone-name
#authentication local-users \
#	radius radius-authentication \

CONFDC_FLAGS_ietf-system=-a ietf-system-ann.yang

include ../include.mk

include ../component.mk

ietf-system.fxs: ietf-system-ann.yang
