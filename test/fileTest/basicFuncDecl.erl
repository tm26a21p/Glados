-module(basicFuncDecl).
-export([start/0,new/1]).

start() ->
   1 + 1.

new(A) ->
    A - 1.