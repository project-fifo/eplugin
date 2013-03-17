-module(example_app).

-export([start/0]).

start() ->
    example_app_app:start([],[]).
