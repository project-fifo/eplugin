-module(eplugin).

-export([start/0,
         discover/0,
         discover/1,
         load/1,
         load/0,
         callbacks/1,
         apply/2,
         apply/1,
         config/1]).

-define(TABLE, plugins).
-define(CONFTABLE, plugin_config).

start() ->
    application:start(sasl),
    application:start(eplugin).

discover() ->
    discover("plugins").

discover(Base) ->
    WC = filename:join([Base, "*", "plugin.conf"]),
    Plugins = filelib:wildcard(WC),
    Loaded = [{filename:dirname(Plugin), file:consult(Plugin)} || Plugin <- Plugins],
    [{Path, Config} || {Path, {ok, [Config]}} <- Loaded].

load(Path) ->
    [load_plugin(P) || P <- discover(Path)].

load() ->
    [load_plugin(P) || P <- discover()].

load_plugin({Path, {Name, Modules, Config}}) ->

    io:format("Loading ~p with modules ~p and config ~p.~n", [Name, Modules, Config]),
    ModuleFiles = [filename:join([Path, M]) || {M, _RegisterFor} <- Modules],
    [compile:file(F, [{outdir, Path}]) || F <- ModuleFiles],
    [code:load_abs(F) || F <- ModuleFiles],
    ets:insert(?CONFTABLE, {Name, Config}),
    [register_callbacks(M) || M <- Modules].

register_callbacks({M, Callbacks}) ->
    Objs = [{Callback, M, Fun} || {Callback, Fun} <- Callbacks ],
    ets:insert(?TABLE, Objs).

callbacks(Name) ->
    [{M, F} || {_, M, F} <- ets:lookup(?TABLE, Name)].

apply(Name) ->
    eplugin:apply(Name, []).

apply(Name, Args) ->
    [erlang:apply(M, F, Args) || {M, F} <- callbacks(Name)].

config(Plugin) ->
    case ets:lookup(?CONFTABLE, Plugin) of
        [] ->
            undefined;
        [{_, Conf}] ->
            {ok, Conf}
    end.
