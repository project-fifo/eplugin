-module(example0).

-export([hello/0,
         enable/1,
         disable/1]).

hello() ->
    io:format("[example0] hello~n").

enable(Conf) ->
    io:format("[example0] we have been enabled!~n~p~n", [Conf]).

disable(Conf) ->
    io:format("[example0] we have been disabled!~n~p~n", [Conf]).
