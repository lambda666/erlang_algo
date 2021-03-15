-module(minfree).
-export([test/0, minfree/1, minfree_v2/1]).

%% make_list(Len, InitValue)
make_list(0, _, List) ->
    List;
make_list(Len, InitValue, List) ->
    make_list(Len-1, InitValue, [InitValue|List]).
make_list(Len, InitValue) ->
    make_list(Len, InitValue, []).

%% set_at(List, Index, Value)
set_at([], _, _, NewList) ->
    lists:reverse(NewList);
set_at([_|Rest], 0, Value, NewList) ->
    set_at(Rest, -1, Value, [Value|NewList]);
set_at([Head|Rest], Index, Value, NewList) ->
    set_at(Rest, Index-1, Value, [Head|NewList]).
set_at(List, Index, Value) ->
    set_at(List, Index, Value, []).

%% set_at fold version
set_at2(List, Index, Value) ->
    F = fun(X, {Cnt, Acc}) ->
        if
            Cnt == Index ->
                {Cnt+1, [Value|Acc]};
            true ->
                {Cnt+1, [X|Acc]}
        end
    end,
    {_, Result} = lists:foldl(F, {0,[]}, List),
    lists:reverse(Result).

%% find_first(List, Value)
find_first([], _, Index) ->
    Index;
find_first([Value|_], Value, Index) ->
    Index;
find_first([_|Rest], Value, Index) ->
    find_first(Rest, Value, Index+1).
find_first(List, Value) ->
    find_first(List, Value, 0).

%% find_first fold version
find_first2(List, Value) ->
    F = fun(X, {Find, Acc}) ->
        if 
            X == Value , Find == -1 ->
                {Acc, Acc+1};
            true ->
                {Find, Acc+1}
        end
    end,
    {Find, _} = lists:foldl(F,{-1, 0}, List),
    Find.

%% check_used(UsedList, FlagList)
check_used([], FlagList) ->
    FlagList;
check_used([Head|Rest], FlagList) ->
    check_used(Rest, set_at2(FlagList, Head, 1)).

%% minfree(List)
minfree(List) ->
    FlagList = make_list(length(List), 0),
    UsedList = check_used(List, FlagList),
    %io:format("~w ~n", [List]),
    %io:format("~w ~n", [UsedList]),
    find_first2(UsedList, 0).

%% {List1, List2} = split(List)
split([], _, _, List1, List2) ->
    {List1, List2};
split([Head|Rest], MinValue, MaxValue, List1, List2) ->
    MiddleValue = (MaxValue - MinValue) div 2 + MinValue,
    if
        Head =< MiddleValue -> 
            split(Rest, MinValue, MaxValue, [Head|List1], List2);
        true ->
            split(Rest, MinValue, MaxValue, List1, [Head|List2])
    end.
split(List, MinValue, MaxValue) ->
    split(List, MinValue, MaxValue, [], []).

%% minfree_v2(List)
minfree_v2([], MinValue, _) ->
    MinValue;
minfree_v2(List, MinValue, MaxValue) ->
    %io:format("~w~n", [List]),
    MiddleValue = (MaxValue - MinValue) div 2 + MinValue,
    %io:format("~w ~w ~w ~n", [MinValue, MiddleValue, MaxValue]),
    {List1, List2} = split(List, MinValue, MaxValue),
    %io:format("~w ~w ~n", [List1, List2]),
    if
        length(List1) == MiddleValue - MinValue + 1 ->
            minfree_v2(List2, MiddleValue + 1, MaxValue);
        true ->
            minfree_v2(List1, MinValue , MiddleValue)
    end.
minfree_v2(List) ->
    minfree_v2(List, 0, length(List)-1).

%% make_arise_list(Len)
make_arise_list(0,List) ->
    List;
make_arise_list(Len, List) ->
    make_arise_list(Len-1, [Len|List]).
make_arise_list(Len) ->
    make_arise_list(Len, []).

test() ->
    List = make_arise_list(10000),
    minfree_v2(List).
    %minfree_v2([0,2,3,4,6,5,5423,7,34]).