
all:	generated.hrl
	$(MAKE) iall

include ../confdvsn.mk
include ../include.mk
include ../stderl.mk



generated.hrl: generated.hrl.src ../confd_lib.vsn ../confd_lib_proto.vsn
	v=`cat ../confd_lib.vsn`; \
	pv=`cat ../confd_lib_proto.vsn`; \
	sed -e "s/%lib_vsn%/$${v}/" -e "s/%lib_proto_vsn%/$${pv}/" $@.src > $@

clean:	iclean
	rm -f generated.hrl


