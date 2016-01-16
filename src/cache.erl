%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Ê®Ò»ÔÂ 2015 20:44
%%%-------------------------------------------------------------------
-module(cache).
-author("Administrator").
-include("cache.hrl").
%% API
-export([start/0]).

%% API2
-export([
    get/2,
    get/3,
    set/2,
    set/3
]).

start() ->
    ok = application:start(cache).

-spec get(TabName, Key) -> term() when
    TabName::atom(),
    Key::term().
get(TabName, Key) ->
    get(TabName, Key, undefined).

-spec get(TabName, Key, DefResult) -> term() when
    TabName::atom(),
    Key::term(),
    DefResult::term().
get(TabName, Key, DefResult) ->
    case ets:lookup(?tabName2etsName(TabName), Key) of
        [] ->
            case ets:lookup(TabName, Key) of
                [] ->
                    DefResult;
                [Result] ->
                    Result
            end;
        [Result] ->
            Result
    end.

-spec set(TabName, Key, Value) -> ok when
    TabName::atom(),
    Key::term(),
    Value::term().
set(TabName, Key, Value) ->
    TabName ! {set,{Key, Value}},
    ok.

-spec set(TabName, Data) -> ok when
    TabName::atom(),
    Data::term().
set(TabName,Data) ->
    TabName ! {set, Data},
    ok.

