-module(ugly_number).

-export([test/0]). 

merge([], [], Result) ->
    lists:reverse(Result);
merge([], [Head2|Rest2], Result) ->
    merge([], Rest2, [Head2|Result]);
merge([Head1|Rest1], [], Result) ->
    merge(Rest1, [], [Head1|Result]);
merge([Head1|Rest1], [Head2|Rest2], Result) ->
    if
        Head1 > Head2 ->
            merge([Head1|Rest1], Rest2, [Head2|Result]);
        Head1 == Head2 ->
            merge(Rest1, Rest2, [Head1|Result]);
        true ->
            merge(Rest1, [Head2|Rest2], [Head1|Result])
    end.
merge(List1, List2) ->
    merge(List1, List2, []).

ugly_number(List, 0) ->
    List;
ugly_number(List, N) ->
    List1 = merge(lists:map(fun(X) -> X*2 end, List), List),
    List2  = merge(lists:map(fun(X) -> X*3 end, List1), List1),
    List3 = merge(lists:map(fun(X) -> X*5 end, List2), List2),
    ugly_number(List3, N - 1).
ugly_number(N) ->
    ugly_number([1], N).



test() ->
    List = ugly_number(2),
    io:format("~w ~n", [List]).
    %merge([1,3, 7], [4,5,6]).