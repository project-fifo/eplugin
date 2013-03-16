-module(example1).

-export([hello/0,
         enable/1,
         disable/1]).

hello() ->
    io:format("[example1] hello~n").

enable(Conf) ->
    io:format("[example1] we have been enabled!~n~p~n", [Conf]).

disable(Conf) ->
    io:format("[example1] we have been disabled!~n~p~n", [Conf]).
