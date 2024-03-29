#!/usr/bin/env escript
-mode(compile).
-compile([sub]).

main(_) ->
    io:format("~p~n", [sub:s(4, 5)]).
