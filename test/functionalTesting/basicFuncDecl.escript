#!/usr/bin/env escript
-mode(compile).
-compile([basicFuncDecl]).

main(_) ->
    io:format("~p~n", [basicFuncDecl:new(basicFuncDecl:start())]).
