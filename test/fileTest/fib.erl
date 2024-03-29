-module(fib).
-export([fib/3]).

fib(a, b, n) ->
    if n < 1 -> a;
    else fib(b, a + b, n-1) end.