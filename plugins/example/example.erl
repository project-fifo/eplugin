-module(example).

-export([hello/0]).

hello() ->
    io:format("hello~n").
