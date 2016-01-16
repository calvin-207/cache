-module(cache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("cache.hrl").
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    PID = cache_sup:start_link(),
    FilePath = code:which(?MODULE),
    BeamPath = filename:dirname(FilePath),
    {ok, BeamList} = file:list_dir(BeamPath),
    NewBeamList =
        lists:foldl(fun(BeamName, BeamAcc) ->
            BaseName = filename:basename(BeamName, ".beam"),
            NewBaseName = erlang:list_to_atom(BaseName),
            case catch NewBaseName:module_info(attribuates) of
                List when erlang:is_list(List) ->
                    case lists:keyfind(behaviour, 1, List) of
                        [] -> BeamAcc;
                        BehaviourList ->
                            case lists:member(cache, BehaviourList) of
                                true ->
                                    [NewBaseName | BeamAcc];
                                fsle ->
                                    BeamAcc
                            end
                    end;
                _ ->
                    BeamAcc
            end
        end, [], BeamList),
    init_cache(NewBeamList),
    PID.
stop(_State) ->
    ok.

init_cache([]) ->
    ok;
init_cache([Beam | BeamList]) ->
    CacheList = Beam:cache(),
    lists:foreach(fun(#cache{tab_name=TabName, cache_name=CacheName}) -> cache_server:start(TabName, CacheName) end, CacheList),
    init_cache(BeamList).