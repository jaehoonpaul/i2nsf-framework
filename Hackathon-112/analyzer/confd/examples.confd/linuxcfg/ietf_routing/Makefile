######################################################################
# (C) 2017 Tail-f Systems
######################################################################

COMPONENT      = ietf_routing
COMPONENT_SRCS = ietf_routing.c ietf_routing_system.c\
                 ietf_routing_system_state.c ietf_routing_subscriber.c
COMPONENT_SRCS += ietf_routing_provider_utils.c
COMPONENT_SRCS += ietf_routing_utils.c ietf_routing_provider.c
COMPONENT_SRCS += ietf_routing_active_route.c

#FXS=ietf-routing.fxs ietf-ipv4-unicast-routing.fxs \
#    ietf-ipv6-unicast-routing.fxs

#TODO RT26183
FXS=ietf-routing.fxs ietf-ipv4-unicast-routing.fxs \
    ietf-ipv6-unicast-routing.fxs

CONFDC_FLAGS_C=--yangpath .:../ietf_interfaces:../ietf_ip
CONFDC_FLAGS_ietf-routing=$(CONFDC_FLAGS_C) -a ietf-routing-ann.yang
CONFDC_FLAGS_ietf-ipv4-unicast-routing=$(CONFDC_FLAGS_C) \
    -a ietf-ipv4-unicast-routing-ann.yang
CONFDC_FLAGS_ietf-ipv6-unicast-routing=$(CONFDC_FLAGS_C) \
    -a ietf-ipv6-unicast-routing-ann.yang

CONFDC_FEATURES_ietf-routing=

include ../include.mk
include ../component.mk
CFLAGS += -I../ietf_interfaces -I../ipmibs

ietf-routing.fxs: ietf-routing-ann.yang
