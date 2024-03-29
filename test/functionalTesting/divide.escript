#!/usr/bin/env escript
-mode(compile).
-compile([divide]).

main(_) ->
    io:format("~p~n", [divide:d(5, 5)]).
