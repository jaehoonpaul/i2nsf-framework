# our standard makefile for erlang applications

include ../include.mk

iall: $(ERL_OBJECTS) $(APP_OBJECTS) docs


docs: ../doc/.docs.ok

../doc/.docs.ok: $(ERL_SOURCES) $(ERL_OBJECTS)
	$(ERL)  -pa ../ebin -noshell -s econfd doc_application \
	"'$(APPNAME)'" '"."' '$(EDOC_OPTS)'
	@touch ../doc/.docs.ok

debug:
	$(MAKE) TYPE=debug


$(ERL_OBJECTS): $(ERL_HEADERS)

clean: iclean

iclean:
	rm -f ../ebin/*.* ../doc/*.html ../doc/.docs.ok *.xso ../priv/*.fxs
