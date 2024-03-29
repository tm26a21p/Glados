#!/usr/bin/env escript
-mode(compile).
-compile([mod]).

main(_) ->
    io:format("~p~n", [mod:m(4, 5)]).
