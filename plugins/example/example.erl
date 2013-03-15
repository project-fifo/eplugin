-module(example).

-export([hello/0,
         init/1,
         enable/1,
         disable/1]).

hello() ->
    io:format("hello~n").

init(Conf) ->
    io:format("we have been initialized!~n~p~n", [Conf]).

enable(Conf) ->
    io:format("we have been enabled!~n~p~n", [Conf]).

disable(Conf) ->
    io:format("we have been disabled!~n~p~n", [Conf]).
