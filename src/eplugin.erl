-module(eplugin).

-include("eplugin.hrl").

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

%% This is a library we don't want xref to spam us with the exported functions.
-ignore_xref([start/0,
              callbacks/1,
              apply/2,
              call/1, call/2, call/3, call/4,
              apply_test/2,
              test/1, test/2, test/3, test/4,
              register/4, register/5,
              fold/2,
              config/1,
              provide/1,
              plugins/0,
              enable/1,
              provide/1,
              is_enabled/1,
              disable/1]).

-export_type([plugin_desc/0]).

%%--------------------------------------------------------------------
%% @type plugin_desc() = {Plugin,
%%                        Modules,
%%                        Config}
%%                    Plugin = atom()
%%                    Module = [module_desc()]
%%                    Config = [plugin_config()].
%%
%% plugin_desc is the description of how plugin.config files are read.
%% Plugin is the name, it should be unique.
%% @end
%%--------------------------------------------------------------------
-type plugin_desc() :: {Plugin::atom(),
                        [module_desc()],
                        [plugin_config()]}.

%%--------------------------------------------------------------------
%% @type module_desc() = {Module,
%%                        Callbacks}
%%                    Module = atom()
%%                    Callback = [callback_desc()].
%%
%% Describes one of the modules of a callback. A module corresponds
%% with a Module.erl file in the plugin directory. A module does not
%% have to register any callbacks, but this directive is needed to
%% trigger compilation. (in other words if you have a utility Module
%% in your plugin register it here just don't list any callbacks.)
%% @end
%%--------------------------------------------------------------------
-type module_desc() :: {Module::atom(), [callback_desc()]}.

%%--------------------------------------------------------------------
%% @type callback_desc() = {Callback,
%%                          Function,
%%                          Config} |
%%                         {Callback,
%%                          Function}
%%                    Callback = atom()
%%                    Function = atom()
%%                    Config = [callback_config()].
%%
%% Registers a function as a callback, the Function must be exported
%% for this to work, optional configuration options can be passed.
%% If the Config is ommitted a empty list is taken.
%% @end
%%--------------------------------------------------------------------
-type callback_desc() :: {Callback::atom(),
                          Function::atom(),
                          Config::[callback_config()]} |
                         {Callback::atom(),
                          Function::atom()}.

%%--------------------------------------------------------------------
%% @type callback_config() = Option
%%                    Option = {priority, integer()}.
%%
%% Options for a callback. The default for priority is 0, a higher
%% priority means the callback gets executed earlery.
%% @end
%%--------------------------------------------------------------------
-type callback_config() :: {priority, integer()}.

%%--------------------------------------------------------------------
%% @type plugin_config() = Option
%%                    Option = {dependencies, [atom()]} |
%%                             {provides, [atom()]} |
%%                             disabled |
%%                             atom() |
%%                             {Key::atom(), Value::any()}.
%%
%% The plugin configuraiton the following values are used by eplugin:
%%  <dl>
%%    <dt><b>dependencies</b></dt>
%%    <dd>this dependecies must be provided, either by another plugin
%%    or by the application itself before the plugin gets enabled
%%    automatically.</dd>
%%    <dt><b>provided</b></dt>
%%    <dd>A list of provided capabilities for other plugins.</dd>
%%    <dt><b>disabled</b></dt>
%%    <dd>This plugin is not enabled automatically.</dd>
%%  </dl>
%% @end
%%--------------------------------------------------------------------

-type plugin_config() :: {dependencies, [atom()]} |
                         {provides, [atom()]} |
                         {add_paths, [file:name_all()]}|
                         disabled |
                         atom() |
                         {Key::atom(), Value::any()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts eplugin.
%%
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    application:start(sasl),
    application:start(lager),
    application:start(eplugin).

%%--------------------------------------------------------------------
%% @doc
%% Lists all registered and enabled callbacks for a given Plugin. Sorted
%% by the <b>priority</b> they were given.
%%
%% @spec callbacks(Plugin::atom()) -> {Module::atom(), Function::atom()}
%% @end
%%--------------------------------------------------------------------
-spec callbacks(Plugin::atom()) ->
                       [{Module::atom(), Function::atom()}].
callbacks(Plugin) ->
    C0 = [{P, M, F} || {_, _, M, F, P} <- ets:lookup(?TABLE, Plugin)],
    [{M, F} || {_, M, F} <- lists:sort(C0)].

%%--------------------------------------------------------------------
%% @doc
%% An equivalten to erlang:apply/2 for callbacks. All enabled callbacks
%% as listed by callbacks/1. Results are retunred in an list.
%%
%% @spec apply(Plugin::atom(), Args::[any()]) -> [any()]
%% @end
%%--------------------------------------------------------------------
-spec apply(Plugin::atom(), Args::[any()]) ->
                   [any()].
apply(Plugin, Args) when is_list(Args) ->
    [erlang:apply(M, F, Args) || {M, F} <- callbacks(Plugin)].

%%--------------------------------------------------------------------
%% @doc
%% Equivalent to eplugin:apply/2 but optimized for calls without
%% arguments by working around erlang:apply and calling the function
%% directly.
%%
%% @spec call(Plugin::atom()) -> [any()]
%% @end
%%--------------------------------------------------------------------
-spec call(Plugin::atom()) ->
                  [any()].
call(Plugin) ->
    [M:F() || {M, F} <- callbacks(Plugin)].

%%--------------------------------------------------------------------
%% @doc
%% See call/1.
%%
%% @spec call(Plugin::atom(), Arg::any()) -> [any()]
%% @end
%%--------------------------------------------------------------------
-spec call(Plugin::atom(), Arg::any()) ->
                  [any()].
call(Plugin, Arg) ->
    [M:F(Arg) || {M, F} <- callbacks(Plugin)].

%%--------------------------------------------------------------------
%% @doc
%% See call/1.
%%
%% @spec call(Plugin::atom(), Arg1::any(), Arg2::any()) -> [any()]
%% @end
%%--------------------------------------------------------------------
-spec call(Plugin::atom(), Arg1::any(), Arg2::any()) ->
                  [any()].
call(Plugin, Arg1, Arg2) ->
    [M:F(Arg1, Arg2) || {M, F} <- callbacks(Plugin)].

%%--------------------------------------------------------------------
%% @doc
%% See call/1.
%%
%% @spec call(Plugin::atom(), Arg1::any(), Arg2::any(), Arg3::any()) -> [any()]
%% @end
%%--------------------------------------------------------------------
-spec call(Plugin::atom(), Arg1::any(), Arg2::any(), Arg3::any()) ->
                  [any()].
call(Plugin, Arg1, Arg2, Arg3) ->
    [M:F(Arg1, Arg2, Arg3) || {M, F} <- callbacks(Plugin)].

%%--------------------------------------------------------------------
%% @doc
%% Executes apply/2 on the results of callbacks/1 as long as true is
%% returned, once another value is returned the execution is stopped
%% and this value returned.
%%
%% @spec apply_test(Plugin::atom(), Args::[any()]) -> true|any()
%% @end
%%--------------------------------------------------------------------
-spec apply_test(Plugin::atom(), Args::[any()]) ->
                        true |
                        any().

apply_test(Plugin, Args) ->
    apply_test_(callbacks(Plugin), Args).

%%--------------------------------------------------------------------
%% @doc
%% Equivalent to eplugin:apply_test/2 but optimized for calls without
%% arguments by working around erlang:apply and calling the function
%% directly.
%%
%% @spec test(Plugin::atom()) -> true|any()
%% @end
%%--------------------------------------------------------------------
-spec test(Plugin::atom()) ->
                  true |
                  any().
test(Plugin) ->
    test_(callbacks(Plugin)).

%%--------------------------------------------------------------------
%% @doc
%% See test/1.
%%
%% @spec test(Plugin::atom(), Arg::any()) -> true|any()
%% @end
%%--------------------------------------------------------------------

-spec test(Plugin::atom(), Arg::any()) -> true|any().

test(Plugin, Arg) ->
    test_(callbacks(Plugin), Arg).

%%--------------------------------------------------------------------
%% @doc
%% See test/1.
%%
%% @spec test(Plugin::atom(), Arg1::any(), Arg2::any()) -> true|any()
%% @end
%%--------------------------------------------------------------------
-spec test(Plugin::atom(), Arg1::any(), Arg2::any()) ->
                  true |
                  any().
test(Plugin, Arg1, Arg2) ->
    test_(callbacks(Plugin), Arg1, Arg2).

%%--------------------------------------------------------------------
%% @doc
%% See test/1.
%%
%% @spec test(Plugin::atom(), Arg1::any(),
%%            Arg2::any(), Arg3::any()) -> true|any()
%% @end
%%--------------------------------------------------------------------
-spec test(Plugin::atom(), Arg1::any(), Arg2::any(), Arg3::any()) ->
                  true |
                  any().

test(Plugin, Arg1, Arg2, Arg3) ->
    test_(callbacks(Plugin), Arg1, Arg2, Arg3).

%%--------------------------------------------------------------------
%% @doc
%% This function folds <b>Acc0</b> through all callbacks for
%% <b>Plugin</b> as returned by callbacks/1. The result of function N is
%% passed as argument to function N+1, N0 is passed Acc0.
%%
%% @spec fold(Plugin::atom(), Acc0::any()) -> any()
%% @end
%%--------------------------------------------------------------------
-spec fold(Plugin::atom(), Acc0::any()) ->
                  any().
fold(Plugin, Acc0) ->
    lists:foldl(fun({M, F}, AccIn) ->
                        M:F(AccIn)
                end, Acc0, callbacks(Plugin)).

%%--------------------------------------------------------------------
%% @doc
%% Returns the configuration of a plugin or undefined when the plugin
%% is not known.
%%
%% @spec config(Plugin::atom()) -> undefined|any()
%% @end
%%--------------------------------------------------------------------
-spec config(Plugin::atom()) ->
                    undefined |
                    plugin_config().
config(Plugin) ->
    case ets:lookup(?CONFTABLE, Plugin) of
        [] ->
            undefined;
        [{_, Conf, _}] ->
            {ok, Conf}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Disables a plugin by removing all callbacks it has registered. The
%% callback eplugin:disable(Config) will be called on the Plugin after
%% it was disabled to allow cleanup. And eplugin:disable_plugin(Plugin)
%% will be called after the plugin was disabled to inform any other
%% interested plugins.
%%
%% @spec disable(Plugin::atom()) -> {error, not_found}|ok
%% @end
%%--------------------------------------------------------------------
-spec disable(Plugin::atom()) ->
                     {error, not_found} |
                     ok.
disable(Plugin) ->
    case is_enabled(Plugin) of
        true ->
            lager:info("[eplugin::~p] disabled.", [Plugin]),
            %% Get all callbacks of this Plugin that are registered for 'eplugin:disable'
            %% this way we can later call them to let the plugin clean up.
            %% We should get this data before we actually disable it since we can use
            %% the registered callbacks table to make our lives easyer.
            Callback = ets:match(?TABLE, {'eplugin:disable', Plugin, '$1', '$2'}),
            ets:match_delete(?TABLE, {'_', Plugin, '_', '_'}),
            {ok, Config} = config(Plugin),
            Base = proplists:get_value(path, Config),
            case proplists:get_value(add_paths, Config, []) of
                [] ->
                    ok;
                Paths ->
                    [code:del_path([Base, "/", P]) || P <- Paths]
            end,
            [M:F(Config) || [M, F] <- Callback],
            eplugin:call('eplugin:disable_plugin', Plugin),
            ok;
        false ->
            lager:warning("[eplugin::~p] already disabled.", [Plugin]),
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Enables a plugin by adding all callbacks it has registered. The
%% callback eplugin:enable(Config) will be called on the Plugin
%% <b>befire</b> it is enabled to allow initialisation. And
%% eplugin:disable_plugin(Plugin) will be called after the plugin is
%% enabled to inform any other interested plugins.
%%
%% @spec enable(Plugin::atom()) -> {error, not_found}|ok
%% @end
%%--------------------------------------------------------------------
-spec enable(Plugin::atom()) ->
                    {error, not_found} |
                    ok.
enable(Plugin) ->
    case is_enabled(Plugin) of
        true ->
            lager:warning("[eplugin::~p] already enabled.", [Plugin]),
            {error, not_found};
        false ->
            case ets:lookup(?CONFTABLE, Plugin) of
                [] ->
                    undefined;
                [{_, Config, Modules}] ->
                    Base = proplists:get_value(path, Config),
                    case proplists:get_value(add_paths, Config, []) of
                        [] ->
                            ok;
                        Paths ->
                            code:add_paths([[Base, "/", P] || P <- Paths])
                    end,
                    %% We look for all functions that are registered to 'eplugin:enable'
                    %% so we can call them before the plugin is enabled.
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

%%--------------------------------------------------------------------
%% @doc
%% Registers Module:Function of the plugin Plugin for Callback. This
%% can be used to programatically hook into callbacks from outside of
%% a plugin.
%%
%% @spec register(Plugin::atom(), Callback::atom(),
%%                Module::atom(), Function::atom(),
%%                Options::callback_config()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec register(Plugin::atom(), Callback::atom(),
               Module::atom(), Function::atom(),
               Options::callback_config()) ->
                      true.
register(Plugin, Callback, Module, Function, Options) ->
    eplugin_srv:register_callback(Plugin, Callback, Module, Function, Options).

%%--------------------------------------------------------------------
%% @doc
%% Calls register/5 with an default empty option set.
%%
%% @spec register(Plugin::atom(), Callback::atom(),
%%                Module::atom(), Function::atom()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec register(Plugin::atom(), Callback::atom(),
               Module::atom(), Function::atom()) ->
                      true.
register(Plugin, Callback, Module, Function) ->
    register(Plugin, Callback, Module, Function, []).

%%--------------------------------------------------------------------
%% @doc
%% Checks if a certain Plugin is enabled.
%%
%% @spec is_enabled(Plugin::atom()) -> true | false
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Plugin::atom()) ->
                        true | false.
is_enabled(Plugin) ->
    case ets:match(?TABLE, {'_', Plugin, '_', '_'}) of
        [] ->
            false;
        _ ->
            true
    end.

%%--------------------------------------------------------------------
%% @doc
%% Lists all installed plugins.
%%
%% @spec plugins() -> [Plugin::atom()]
%% @end
%%--------------------------------------------------------------------

-spec plugins() -> [Plugin::atom()].

plugins() ->
    [P || [P] <- ets:match(?CONFTABLE, {'$1', '_', '_'})].

%%--------------------------------------------------------------------
%% @doc
%% Registers a certain dependency as provided. This is used for the
%% loading process of plugins.
%%
%% @spec provide(What::atom()) -> ok
%% @end
%%--------------------------------------------------------------------

-spec provide(What::atom()) -> ok.

provide(What) ->
    eplugin_srv:provide(What).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% apply_test/2 recursion function.
%%
%% @spec apply_test_([{Module::atom(), Function::atom()}],
%%                   Args::[any()]) -> true|any()
%% @end
%%--------------------------------------------------------------------

-spec apply_test_([{Module::atom(), Function::atom()}], Args::[any()]) -> true|any().

apply_test_([], _) ->
    true;

apply_test_([{M, F} | Cs], Args) ->
    case erlang:apply(M, F, Args) of
        true ->
            apply_test_(Cs, Args);
        R ->
            R
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% test/1 recursion function.
%%
%% @spec test_([{Module::atom(), Function::atom()}]) -> true|any()
%% @end
%%--------------------------------------------------------------------

-spec test_([{Module::atom(), Function::atom()}]) -> true|any().

test_([]) ->
    true;

test_([{M, F} | Cs]) ->
    case M:F() of
        true ->
            test_(Cs);
        R ->
            R
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% test/2 recursion function.
%%
%% @spec test_([{Module::atom(), Function::atom()}],
%%             Arg1::any()) -> true|any()
%% @end
%%--------------------------------------------------------------------

-spec test_([{Module::atom(), Function::atom()}],
            Arg1::any()) -> true|any().

test_([], _) ->
    true;

test_([{M, F} | Cs], Arg) ->
    case M:F(Arg) of
        true ->
            test_(Cs, Arg);
        R ->
            R
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% test/3 recursion function.
%%
%% @spec test_([{Module::atom(), Function::atom()}],
%%             Arg1::any(), Arg2::any()) -> true|any()
%% @end
%%--------------------------------------------------------------------
-spec test_([{Module::atom(), Function::atom()}],
            Arg1::any(), Arg2::any()) ->
                   true|
                   any().
test_([], _, _) ->
    true;

test_([{M, F} | Cs], Arg1, Arg2) ->
    case M:F(Arg1, Arg2) of
        true ->
            test_(Cs, Arg1, Arg2);
        R ->
            R
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% test/3 recursion function.
%%
%% @spec test_([{Module::atom(), Function::atom()}],
%%             Arg1::any(), Arg2::any(), Arg3::any()) -> true|any()
%% @end
%%--------------------------------------------------------------------
-spec test_([{Module::atom(), Function::atom()}],
            Arg1::any(), Arg2::any(), Arg3::any()) ->
                   true|
                   any().
test_([], _, _, _) ->
    true;

test_([{M, F} | Cs], Arg1, Arg2, Arg3) ->
    case M:F(Arg1, Arg2, Arg3) of
        true ->
            test_(Cs, Arg1, Arg2, Arg3);
        R ->
            R
    end.
