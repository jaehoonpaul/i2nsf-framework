######################################################################
# (C) 2014 Tail-f Systems
######################################################################

NS_PREFIX  ?= $(COMPONENT)

COMPONENT_SRCS ?= $(COMPONENT).c
COMPONENT_OBJS  = $(COMPONENT_SRCS:%.c=%.o)
FXS            ?= $(COMPONENT).fxs
FXS_H           = $(FXS:%.fxs=%.h)

all:	$(COMPONENT).a($(COMPONENT_OBJS)) $(FXS)

fxs:    $(FXS) $(FXS_H)

%.fxs: %.yang
	$(CONFDC) $(CONFDC_FLAGS) $(CONFDC_FEATURES_$*:%=-F %) \
	 $(CONFDC_FLAGS_$*) -c \
		--yangpath ..:$(CONFD_DIR)/src/confd/snmp/yang -o $@ $<


%.bin: %.mib %.fxs
	$(CONFDC) -o $@ -c $< $(shell echo $^ | \
	sed 's/[^ ]*\.\(bin\|miba\?\)//g') \
		-f . \
		-f $(CONFD_DIR)/etc/confd \
		-f $(CONFD_DIR)/etc/confd/snmp \
	$(addprefix --mib-annotation , $(wildcard $(@:%.bin=%.miba)))

$(COMPONENT).a($(COMPONENT_OBJS)): $(COMPONENT_OBJS)
	$(AR) rU $@ $?

$(COMPONENT_OBJS): $(FXS_H) ../linuxcfg_api.h

clean:
ifneq ($(PREFIX_INSTALLDIR),$(CONFD_DIR))
	-rm -rf $(PREFIX_INSTALLDIR)
endif
	-rm -f *~ *.o *.a *.fxs $(FXS_H) 2> /dev/null

install: install_fxs

install_fxs:
	@mkdir -p $(CONFD_INSTALLDIR)
ifeq ($(KEEP_EXISTING),yes)
	@for f in $(FXS); do \
		if [ -f $(CONFD_INSTALLDIR)/$$f ]; then \
		echo "$(CONFD_INSTALLDIR)/$$f already exists, keeping!"; \
		else \
			@install -m 0644 $$f $(CONFD_INSTALLDIR); \
		fi; \
	done
else
	@install -m 0644 $(FXS) $(CONFD_INSTALLDIR)
endif

install_init_xml:

show_globals: $(COMPONENT_OBJS)
	nm $(COMPONENT_OBJS) | egrep '( [A-Zuvw] )|(^\w+.o:)' \
		| egrep -v ' [UC] '

# To allow overwriting the rules above and/or adding new ones
ifdef INCLUDE_COMPONENT_MK
include $(INCLUDE_COMPONENT_MK)
endif
