-module(factorial).
-export([factorial/1]).

factorial(X) ->
    if X == 1 -> 1;
    else X * factorial(X - 1) end.