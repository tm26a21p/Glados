#!/usr/bin/env escript
-mode(compile).
-compile([fib]).

main(_) ->
    io:format("~p~n", [fib:fib(0, 1, 20)]).
