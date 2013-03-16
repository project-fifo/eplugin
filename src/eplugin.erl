-module(eplugin).

-export([start/0,
         callbacks/1,
         apply/2,
         apply/1,
         config/1,
         plugins/0,
         enable/1,
         is_enabled/1,
         disable/1]).

-define(TABLE, plugins).
-define(CONFTABLE, plugin_config).

start() ->
    application:start(sasl),
    application:start(lager),
    application:start(eplugin).

callbacks(Name) ->
    [{M, F} || {_, _, M, F} <- ets:lookup(?TABLE, Name)].

apply(Name) ->
    eplugin:apply(Name, []).

apply(Name, Args) ->
    [erlang:apply(M, F, Args) || {M, F} <- callbacks(Name)].

config(Plugin) ->
    case ets:lookup(?CONFTABLE, Plugin) of
        [] ->
            undefined;
        [{_, Conf, _}] ->
            {ok, Conf}
    end.

disable(Plugin) ->
    case is_enabled(Plugin) of
        true ->
            lager:info("[eplugin::~p] disabled.", [Plugin]),
            Callback = ets:match(?TABLE, {'eplugin:disable', Plugin, '$1', '$2'}),
            ets:match_delete(?TABLE, {'_', Plugin, '_', '_'}),
            {ok, Config} = config(Plugin),
            [erlang:apply(M, F, [Config]) || [M, F] <- Callback],
            eplugin:apply('eplugin:disable_plugin', [Plugin]),
            ok;
        false ->
            lager:warning("[eplugin::~p] already disabled.", [Plugin])
    end.

enable(Plugin) ->
    case is_enabled(Plugin) of
        true ->
            lager:warning("[eplugin::~p] already enabled.", [Plugin]);
        false ->
            case ets:lookup(?CONFTABLE, Plugin) of
                [] ->
                    undefined;
                [{_, _, Modules}] ->
                    {ok, Config} = config(Plugin),
                    lists:foreach(fun({M, Callbacks}) ->
                                          lists:foreach(fun({'eplugin:enable', Fun}) ->
                                                                erlang:apply(M, Fun, [Config]);
                                                           (_) ->
                                                                ok
                                                        end, Callbacks)
                                  end, Modules),
                    [eplugin_srv:register_callbacks(Plugin, M) || M <- Modules],
                    eplugin:apply('eplugin:enable_plugin', [Plugin]),
                    [eplugin_srv:provide(What) || What <- proplists:get_value(provides, Config, [])],
                    ok
            end
    end.

is_enabled(Plugin) ->
    case ets:match(?TABLE, {'_', Plugin, '_', '_'}) of
        [] ->
            false;
        _ ->
            true
    end.

plugins() ->
    [P || [P] <- ets:match(?CONFTABLE, {'$1', '_', '_'})].
