######################################################################
# (C) 2014 Tail-f Systems
######################################################################

COMPONENT      = ietf_interfaces
COMPONENT_SRCS = ietf_interfaces.c dyn_iface.c

FXS=iana-if-type.fxs ietf-interfaces.fxs
CONFDC_FEATURES_ietf-interfaces=
CONFDC_FLAGS_ietf-interfaces= -a ietf-interfaces-ann.yang

include ../include.mk

include ../component.mk

CFLAGS += -DDYN_IFACE -I../ipmibs

ietf-interfaces.fxs: ietf-interfaces-ann.yang
