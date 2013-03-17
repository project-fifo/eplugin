-module(eplugin).

-export([start/0,
         callbacks/1,
         apply/2,
         call/1, call/2, call/3, call/4,
         apply_test/2,
         test/1, test/2, test/3, test/4,
         register/4, register/5,
         fold/2,
         config/1,
         plugins/0,
         enable/1,
         provide/1,
         is_enabled/1,
         disable/1]).

-define(TABLE, plugins).
-define(CONFTABLE, plugin_config).

start() ->
    application:start(sasl),
    application:start(lager),
    application:start(eplugin).

callbacks(Name) ->
    C0 = [{P, M, F} || {_, _, M, F, P} <- ets:lookup(?TABLE, Name)],
    [{M, F} || {_, M, F} <- lists:sort(C0)].

apply(Name, Args) when is_list(Args) ->
    [erlang:apply(M, F, Args) || {M, F} <- callbacks(Name)].


call(Name) ->
    [M:F() || {M, F} <- callbacks(Name)].

call(Name, Arg) ->
    [M:F(Arg) || {M, F} <- callbacks(Name)].

call(Name, Arg1, Arg2) ->
    [M:F(Arg1, Arg2) || {M, F} <- callbacks(Name)].

call(Name, Arg1, Arg2, Arg3) ->
    [M:F(Arg1, Arg2, Arg3) || {M, F} <- callbacks(Name)].

apply_test(Name, Args) ->
    apply_test_(callbacks(Name), Args).

apply_test_([], _) ->
    true;

apply_test_([{M, F} | Cs], Args) ->
    case erlang:apply(M, F, Args) of
        true ->
            apply_test_(Cs, Args);
        R ->
            R
    end.

test(Name) ->
    test_(callbacks(Name)).

test_([]) ->
    true;

test_([{M, F} | Cs]) ->
    case M:F() of
        true ->
            test_(Cs);
        R ->
            R
    end.

test(Name, Arg) ->
    test_(callbacks(Name), Arg).

test_([], _) ->
    true;

test_([{M, F} | Cs], Arg) ->
    case M:F(Arg) of
        true ->
            test_(Cs, Arg);
        R ->
            R
    end.

test(Name, Arg1, Arg2) ->
    test_(callbacks(Name), Arg1, Arg2).

test_([], _, _) ->
    true;

test_([{M, F} | Cs], Arg1, Arg2) ->
    case M:F(Arg1, Arg2) of
        true ->
            test_(Cs, Arg1, Arg2);
        R ->
            R
    end.

test(Name, Arg1, Arg2, Arg3) ->
    test_(callbacks(Name), Arg1, Arg2, Arg3).

test_([], _, _, _) ->
    true;

test_([{M, F} | Cs], Arg1, Arg2, Arg3) ->
    case M:F(Arg1, Arg2, Arg3) of
        true ->
            test_(Cs, Arg1, Arg2, Arg3);
        R ->
            R
    end.

fold(Name, Acc0) ->
    lists:foldl(fun({M, F}, AccIn) ->
                        M:F(AccIn)
                end, Acc0, callbacks(Name)).

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
            [M:F(Config) || [M, F] <- Callback],
            eplugin:call('eplugin:disable_plugin', Plugin),
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
                                                                M:Fun(Config);
                                                           (_) ->
                                                                ok
                                                        end, Callbacks)
                                  end, Modules),
                    [eplugin_srv:register_callbacks(Plugin, M) || M <- Modules],
                    eplugin:call('eplugin:enable_plugin', Plugin),
                    [eplugin_srv:provide(What) || What <- proplists:get_value(provides, Config, [])],
                    ok
            end
    end.

register(Name, Callback, Module, Function) ->
    register(Name, Callback, Module, Function, []).

register(Name, Callback, Module, Function, Options) ->
    eplugin_srv:register_callback(Name, Callback, Module, Function, Options).

is_enabled(Plugin) ->
    case ets:match(?TABLE, {'_', Plugin, '_', '_'}) of
        [] ->
            false;
        _ ->
            true
    end.

plugins() ->
    [P || [P] <- ets:match(?CONFTABLE, {'$1', '_', '_'})].

provide(What) ->
    eplugin_srv:provide(What).
