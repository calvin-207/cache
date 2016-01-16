%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2015 20:43
%%%-------------------------------------------------------------------
-module(cache_server).
-author("Administrator").

-behaviour(gen_server).

%% API
-export([start/0,
    start/2,
    start_link/2
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start(manager, manager).

start(TabName, CacheName) ->
    supervisor:start_child(cache_sup, {?MODULE, start_link, [TabName, CacheName]}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term(), term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(TabName, CacheName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [TabName, CacheName], []).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([TabName, CacheName]) ->
    set_tab_name(TabName),
    set_cache_name(CacheName),
    case TabName of
        manager ->
            erlang:send_after(300000, self(), send_time_to_write);
        _ ->
            ignore
    end,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
    catch handle(Info),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    dump(all),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_cache_name() ->
    erlagn:get(cache_name).
set_cache_name(CacheName) ->
    erlang:put(cache_name, CacheName).

set_tab_name(TabName) ->
    erlang:put(tab_name,TabName).
get_tab_name() ->
    erlang:get(tab_name).

get_tab_list() ->
    case erlang:get(tab_list) of
        [] ->
            ets:lookup(get_tab_name(), tab_list);
        List ->
            List
    end.
set_tab_list(TabList) ->
    erlang:put(tab_list,TabList).

%%%===================================================================

%%%===================================================================

handle({set, Data}) ->
    ets:insert(get_tab_name(), Data);

%% 一次写回去数据的个数，可以扩展为支持动态配置的
handle(write) ->
    dump(30);

handle(send_time_to_write) ->
    [TabName | Others] = get_tab_list(),
    erlang:send(TabName, write),
    set_tab_list(Others),
    erlang:send_after(300000, self(), send_time_to_write).

dump(all) ->
    TabName = get_tab_name(),
    CacheName = get_cache_name(),
    DataList =ets:tab2list(CacheName),
    lists:foreach(fun(Data) -> mnesia:dirty_write(TabName, Data) end, DataList);
dump(0) ->
    ok;
dump(Length) ->
    TabName = get_tab_name(),
    CacheName = get_cache_name(),
    case ets:first(CacheName) of
        '$end_of_table' ->
            ok;
        Key ->
            Data = ets:lookup(CacheName, Key),
            mnesia:dirty_write(TabName, Data),
            ets:delete(CacheName, Key),
            dump(Length - 1)
    end.

