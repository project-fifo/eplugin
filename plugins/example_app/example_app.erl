-module(example_app).

-export([start/1]).

start(_Config) ->
    example_app_app:start(ok,ok).
