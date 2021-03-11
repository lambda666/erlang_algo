-module(minfree).
-export([test/0, minfree/1]).

%% make_list(Len, InitValue)
make_list(0, _, List) ->
    List;
make_list(Len, InitValue, List) ->
    make_list(Len-1, InitValue, [InitValue|List]).
make_list(Len, InitValue) ->
    make_list(Len, InitValue, []).

%% reverse(List)
reverse([], NewList) ->
    NewList;
reverse([Head|Rest], NewList) ->
    reverse(Rest, [Head|NewList]).
reverse(List) ->
    reverse(List, []).

%% set_at(List, Index, Value)
set_at([], _, _, NewList) ->
    reverse(NewList);
set_at([_|Rest], 0, Value, NewList) ->
    set_at(Rest, -1, Value, [Value|NewList]);
set_at([Head|Rest], Index, Value, NewList) ->
    set_at(Rest, Index-1, Value, [Head|NewList]).
set_at(List, Index, Value) ->
    set_at(List, Index, Value, []).

%% find_first(List, Value)
find_first([], _, Index) ->
    Index;
find_first([Value|_], Value, Index) ->
    Index;
find_first([_|Rest], Value, Index) ->
    find_first(Rest, Value, Index+1).
find_first(List, Value) ->
    find_first(List, Value, 0).

%% check_used(UsedList, FlagList)
check_used([], FlagList) ->
    FlagList;
check_used([Head|Rest], FlagList) ->
    check_used(Rest, set_at(FlagList, Head, 1)).

%% minfree(List)
minfree(List) ->
    FlagList = make_list(length(List), 0),
    UsedList = check_used(List, FlagList),
    io:format("~w ~n", [List]),
    io:format("~w ~n", [UsedList]),
    find_first(UsedList, 0).

test() ->
    minfree([0,1,2,3,4,6]).