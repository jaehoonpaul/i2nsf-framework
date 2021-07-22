-module(starter).

-on_load(on_load/0).

on_load() ->
    test:istart(),
    ok.
