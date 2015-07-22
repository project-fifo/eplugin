%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(eplugin_srv).

-behaviour(gen_server).

-include("eplugin.hrl").

%% API
-export([start_link/0]).
-ignore_xref([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([provide/1, wait_until_provided/1, register_callbacks/2, register_callback/5, reload/0]).

-define(SERVER, ?MODULE).

-record(state, {pending = [], provided = [], waiting = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec provide(What::atom()) ->
                     ok.
provide(What) ->
    gen_server:cast(?SERVER, {provide, What}).

-spec reload() ->
                    ok.
reload() ->
    gen_server:cast(?SERVER, reload).


-spec wait_until_provided(What::atom()) -> ok.
wait_until_provided(What) ->
    gen_server:call(?SERVER, {wait_until_provided, What}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?TABLE, [bag, {read_concurrency, true}, named_table, public]),
    ets:new(?CONFTABLE, [set, {read_concurrency, true}, named_table, public]),
    {ok, Base} = application:get_env(plugin_dir),
    WC = filename:join([Base, "*", "plugin.conf"]),
    Configs = filelib:wildcard(WC),
    Loaded = lists:foldl(fun(File, Acc) ->
                                 case load_config(File) of
                                     {ok, Path, Name, Modules, Config} ->
                                         [{Path, Name, Modules, Config} | Acc];
                                     _ ->
                                         Acc
                                 end
                         end, [], Configs),
    provide(eplugin),
    {ok, #state{pending = Loaded}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({wait_until_provided, What}, From, State = #state{provided = Provided,
                                                              waiting = Waiting}) ->
    case ordsets:is_element(What, Provided) of
        true ->
            {reply, ok, State};
        _ ->
            Waiting1 = [{What, From} | Waiting],
            {noreply, State#state{waiting = Waiting1}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(reload, State = #state{pending = Pending}) ->
    {ok, Base} = application:get_env(plugin_dir),
    WC = filename:join([Base, "*", "plugin.conf"]),
    Plugins = eplugin:plugins(),
    Configs = filelib:wildcard(WC),
    New = lists:foldl(fun(File, Acc) ->
                              case load_config(File) of
                                  {ok, Path, Name, Modules, Config} ->
                                      case lists:keyfind(Name, 2, Plugins) of
                                          false ->
                                              [{Path, Name, Modules, Config} | Acc];
                                          _ ->
                                              Acc
                                      end;
                                  _ ->
                                      Acc
                              end
                      end, [], Configs),
    provide(eplugin),
    {ok, State#state{pending = Pending ++ New}};

handle_cast({provide, What}, State = #state{pending = Pending,
                                            provided = Provided,
                                            waiting = Waiting}) ->
    lager:info("[eplugin] ~p is now provided.", [What]),
    Provided1 = ordsets:add_element(What,  Provided),
    {Load, Pending1} = lists:partition(fun({_Path, _Name, _Modules, Config}) ->
                                               Deps = ordsets:from_list(proplists:get_value(dependencies, Config, [])),
                                               ordsets:is_subset(Deps, Provided1)
                                       end, Pending),
    lists:foreach(fun({Path, Name, Modules, Config}) ->
                          compile_plugin(Path, Name, Modules, Config)
                  end, Load),
    {Ready, Waiting1} = lists:partition(fun({WaitWhat, _WaitFrom}) ->
                                                WaitWhat =:= What
                                        end, Waiting),
    lists:foreach(fun({_, WaitFrom}) ->
                          gen_server:reply(WaitFrom, ok)
                  end, Ready),
    {noreply, State#state{provided = Provided1, pending = Pending1, waiting = Waiting1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_plugin(Path, Name, Modules, Config) ->
    case compile_modules(Name, Path, Modules) of
        error ->
            {error, Name};
        ok ->
            case load_modules(Name, Path, Modules) of
                error ->
                    {error, Name};
                ok ->
                    ets:insert(?CONFTABLE, {Name, [{path, Path} | Config], Modules}),
                    case proplists:get_value(disabled, Config) of
                        true ->
                            lager:warning("[eplugin::~p] Disabled.", [Name]);
                        _ ->
                            eplugin:enable(Name)
                    end,
                    {ok, Name}
            end
    end.


load_modules(Name, _Path, []) ->
    lager:info("[eplugin::~p] All module loaded loaded successfuly.", [Name]),
    ok;

load_modules(Name, Path, [{M, _RegisterFor} | Modules]) ->
    File = filename:join([Path, M]),
                                                %    code:delete(M),
                                                %    code:purge(M),
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
    lager:info("[eplugin::~p] All modules compiled successfuly.", [Name]),
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
    case compile:file(File, [{outdir, Path}, warnings_as_errors]) of
        error ->
            lager:error("[eplugin::~p] Compiling failed.", [Name]),
            {error, unknown};
        {error, Errors, Warnings} ->
            lager:error("[eplugin::~p] Compiling failed. Errors: ~p Warnings: ~p.", [Name, Errors, Warnings]),
            {error, {Errors, Warnings}};
        {ok, ModuleName} ->
            lager:info("[eplugin::~p] Compiling successful of ~p.", [Name, ModuleName]),
            {ok, ModuleName};
        {ok, ModuleName,Warnings} ->
            lager:info("[eplugin::~p] Compiling successful of ~p with warnings: ~p.", [Name, ModuleName, Warnings]),
            {ok, ModuleName}
    end.

register_callbacks(Name, {Module, Callbacks}) ->
    [case C of
         {Callback, Function, Options} ->
             register_callback(Name, Callback, Module, Function, Options);
         {Callback, Function} ->
             register_callback(Name, Callback, Module, Function, [])
     end || C <- Callbacks ].

-spec register_callback(Name::atom(),
                        Callback::atom(),
                        Module::atom(),
                        Function::atom(),
                        Options::[proplists:property()]) ->
                               true.

register_callback(Name, Callback, Module, Function, Options) when
      is_atom(Name), is_atom(Callback), is_atom(Module), is_atom(Function),
      is_list(Options) ->
    Priority = proplists:get_value(priority, Options, 0),
    lager:info("[eplugin::~p] Registering callback ~p with ~p:~p.", [Name, Callback, Module, Function]),
    %% To get a propper sorting order we take the negative priority value
    ets:insert(?TABLE, {Callback, Name, Module, Function, Priority*-1}).

load_config(ConfigFile) ->
    Path = filename:dirname(ConfigFile),
    lager:info("[plugin] Loading plugin from ~s", [Path]),
    case file:consult(ConfigFile) of
        {ok, [{Name, Modules, Config}]} ->
            lager:info("[eplugin::~s] Config loaded.", [Name]),
            {ok, Path, Name, Modules, Config};
        {ok, Other} ->
            lager:info("[eplugin] failed to load plugin config ~s with error: ~p", [ConfigFile, {invalid, Other}]),           {error, {invalid, Other}};
        {error, Reason} ->
            lager:info("[eplugin] failed to load plugin config ~s with error: ~p", [ConfigFile, Reason]),
            {error, Reason}
    end.
