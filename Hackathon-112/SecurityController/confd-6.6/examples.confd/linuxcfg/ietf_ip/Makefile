######################################################################
# (C) 2014 Tail-f Systems
######################################################################

COMPONENT      = ietf_ip
COMPONENT_SRCS = ietf_ip.c iflist.c ipv4.c ipv6.c

FXS=ietf-ip.fxs
CONFDC_FLAGS_ietf-ip=--yangpath ../ietf_interfaces -a ietf-ip-ann.yang\

CONFDC_FEATURES_ietf-ip=ipv6-privacy-autoconf


include ../include.mk

include ../component.mk

CFLAGS += -I../ietf_interfaces -I ../ipmibs

ietf-ip.fxs: ietf-ip-ann.yang
