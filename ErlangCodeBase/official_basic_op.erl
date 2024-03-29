-module(basic_op).
-export([add/2, sub/2, mul/2, divide/2, mod/2]).

sub(A, B) ->
    A - B.

add(A, B) ->
    A + B.

mul(A, B) ->
    A * B.

divide(A, B) ->
    A div B.

mod(A, B) ->
    A rem B.
