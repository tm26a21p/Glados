#!/usr/bin/env escript
-mode(compile).
-compile([mul]).

main(_) ->
    io:format("~p~n", [mul:m(4, 5)]).
