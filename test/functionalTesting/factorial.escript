#!/usr/bin/env escript
-mode(compile).
-compile([factorial]).

main(_) ->
    io:format("~p~n", [factorial:factorial(3)]).
