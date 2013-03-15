-module(eplugin).

-export([start/0,
         discover/0,
         discover/1,
         load/1,
         load/0,
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

discover() ->
    discover("plugins").

discover(Base) ->
    lager:info("[eplugin] discovering plugins in ~s", [Base]),
    WC = filename:join([Base, "*", "plugin.conf"]),
    filelib:wildcard(WC).

load(Path) ->
    Res = [load_plugin(P) || P <- discover(Path)],
    init(),
    Res.

load() ->
    Res = [load_plugin(P) || P <- discover()],
    init(),
    Res.

load_plugin(ConfigFile) ->
    %% Loaded = [{filename:dirname(Plugin), file:consult(Plugin)} || Plugin <- Plugins],
    %% [{Path, Config} || {Path, {ok, [Config]}} <- Loaded].

    Path = filename:dirname(ConfigFile),
    lager:info("[plugin] Loading plugin from ~s", [Path]),
    case file:consult(ConfigFile) of
        {ok, [{Name, Modules, Config}]} ->
            lager:info("[eplugin::~s] Config loaded.", [Name]),
            case compile_modules(Name, Path, Modules) of
                error ->
                    {error, Name};
                ok ->
                    case load_modules(Name, Path, Modules) of
                        error ->
                            {error, Name};
                        ok ->
                            case proplists:get_value(disabled, Config) of
                                true ->
                                    lager:warning("[eplugin::~p] Disabled.", [Name]);
                                _ ->
                                    [register_callbacks(Name, M) || M <- Modules]
                            end,
                            ets:insert(?CONFTABLE, {Name, Config, Modules}),
                            {ok, Name}
                    end
            end;
        {ok, Other} ->
            lager:info("[eplugin] failed to load plugin config ~s with error: ~p", [ConfigFile, {invalid, Other}]),           {error, {invalid, Other}};
        {error, Reason} ->
            lager:info("[eplugin] failed to load plugin config ~s with error: ~p", [ConfigFile, Reason]),
            {error, Reason}
    end.

load_modules(Name, _Path, []) ->
    lager:info("[eplugin::~p] All module loaded loaded successfuly.", [Name]),
    ok;

load_modules(Name, Path, [{M, _RegisterFor} | Modules]) ->
    File = filename:join([Path, M]),
    case code:load_abs(File) of
        {error, Reason} ->
            lager:error("[eplugin::~p] Failed to load module ~p(~s): ~p.", [Name, M, File, Reason]),
            lager:error("[eplugin::~p] Aborting initialisation.", [Name]),
            error;
        _ ->
            lager:info("[eplugin::~p] Module loaded ~p(~s).", [Name, M, File]),
            load_modules(Name, Path, Modules)
    end.

compile_modules(Name, _Paht, []) ->
    lager:info("[eplugin::~p] All modules compiled successfully.", [Name]),
    ok;

compile_modules(Name, Path, [{M, _RegisterFor} | Modules]) ->
    case compile_module(Name, Path, M) of
        {ok, _} ->
            compile_modules(Name, Path, Modules);
        {error, _} ->
            lager:error("[eplugin::~p] Aborting initialisation.", [Name]),
            error
    end.

compile_module(Name, Path, Module) ->
    File = filename:join([Path, Module]),
    lager:info("[eplugin::~p] Compiling module ~s", [Name, File]),
    case compile:file(File, [{outdir, Path}]) of
        error ->
            lager:error("[eplugin::~p] Compiling failed.", [Name]),
            {error, unknown};
        {error, Errors, Warnings} ->
            lager:error("[eplugin::~p] Compiling failed. Errors: ~p Warnings: ~p.", [Name, Errors, Warnings]),
            {error, {Errors, Warnings}};
        {ok, ModuleName} ->
            lager:info("[eplugin::~p] Compiling successfull of ~p.", [Name, ModuleName]),
            {ok, ModuleName};
        {ok, ModuleName,Warnings} ->
            lager:info("[eplugin::~p] Compiling successfull of ~p with warnings: ~p.", [Name, ModuleName, Warnings]),
            {ok, ModuleName}
    end.

register_callbacks(Name, {Module, Callbacks}) ->
    [register_callback(Name, Callback, Module, Function) || {Callback, Function} <- Callbacks ].

register_callback(Name, Callback, Module, Function) ->
    lager:info("[eplugin::~p] Registering callback ~p with ~p:~p.", [Name, Callback, Module, Function]),
    ets:insert(?TABLE, {Callback, Name, Module, Function}).

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
                    [register_callbacks(Plugin, M) || M <- Modules],
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


init() ->
    case ets:match(?TABLE, {'eplugin:init', '$1', '$2', '$3'}) of
        [] ->
            ok;
        Inits ->
            IFn = fun(P, M, F) ->
                          {ok, Config} = config(P),
                          erlang:apply(M, F, [Config])
                  end,
            [ IFn(P, M, F) || [P, M, F] <- Inits],
            true
    end.
