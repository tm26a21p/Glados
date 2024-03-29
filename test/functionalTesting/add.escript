#!/usr/bin/env escript
-mode(compile).
-compile([add]).

main(_) ->
    io:format("~p~n", [add:a(4, 5)]).
