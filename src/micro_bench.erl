-module(micro_bench).

-export([measure/1, compare/2]).

-type bench_def() ::#{name => atom() | string() | binary(),
                      function => fun(),
                      args => list(),
                      initialize => fun(),
                      clean => fun()}.

-spec measure(bench_def()) -> {float(), float()}.
measure(Val) ->
    measure(Val, false).

-spec measure(bench_def(), boolean()) -> {float(), float()}.
measure(#{function := Fun, args := Args} = Def, Print) ->
    NArgs = case maps:get(initialize, Def, none) of
                none ->
                    Args;
                Init ->
                    Init(Args)
            end,
    {Iters, Time} = find_optimal_iterations(Fun, NArgs, 1, 100000),
    TestCounts = if 3*Time > 320000 ->
                         3;
                    true ->
                         round(32*Time/100000)
                 end,
    Res = measure_helper(Fun, NArgs, Iters, [], 1, TestCounts, Print),
    case maps:get(clean, Def, none) of
        none ->
            ok;
        Clean ->
            Clean(NArgs)
    end,
    Res.

-spec compare(string()|binary(), list(bench_def())) -> ok.
compare(Label, Definitions) ->
    Defs = lists:map(fun(#{name := Name} = Def) -> Def#{name := to_binary(Name)} end, Definitions),
    NameWidth = lists:max(lists:map(fun(#{name:=N}) -> size(N)  end, Defs)),
    Times = lists:map(fun(#{name:=Name} = Def) ->
                              {Time, StdDev} = measure(Def, true),
                              SName = io_lib:format("~*.. s", [NameWidth, Name]),
                              STime = io_lib:format("~.2f+-~.2f us", [Time, StdDev]),
                              io:format("~s ~s : ~s~n", [color:blue(SName), Label, color:magenta(STime)]),
                              Time
                      end, Defs),
    Min = lists:min(Times),
    Strs = lists:map(fun({#{name:=Name}, Time}) when Time == Min ->
                             io_lib:format("~s ~s", [color:blue(Name), color:green("fastest")]);
                        ({#{name:=Name}, Time}) ->
                             Slower = io_lib:format("~.1f% slower", [(Time-Min)/Min*100]),
                             io_lib:format("~s ~s", [color:blue(Name), color:red(iolist_to_binary(Slower))])
                     end, lists:zip(Defs, Times)),
    io:format("Compared: ~s~n~n", [string:join(Strs, ", ")]).

-spec to_binary(atom()|string()|binary()) -> binary().
to_binary(Name) when is_atom(Name) ->
    iolist_to_binary(atom_to_list(Name));
to_binary(Name) when is_list(Name) ->
    iolist_to_binary(Name);
to_binary(Name) ->
    Name.

-spec find_optimal_iterations(fun(), list(), number(), number()) -> {number(), number()}.
find_optimal_iterations(Fun, Attrs, Iter, ExpTime) ->
    {Time, _Res} = timer:tc(fun loop/3, [Fun, Attrs, Iter]),
    if Time == 0 ->
            find_optimal_iterations(Fun, Attrs, Iter*10, ExpTime);
       Time > ExpTime*2 ->
            if Iter < 3 ->
                    {Iter, Time};
               true ->
                    find_optimal_iterations(Fun, Attrs, round(ExpTime/Time * Iter), ExpTime)
            end;
       Time < ExpTime/2 ->
            find_optimal_iterations(Fun, Attrs, round(ExpTime/Time * Iter), ExpTime);
       true ->
            {Iter, Time}
    end.

-spec measure_helper(fun(), list(), number(), any(), number(), number(), boolean()) -> {float(), float()}.
measure_helper(_Fun, _Attrs, Iter, Results, Max, Max, Print) ->
    if Print -> io:format("\r                      \r", []);
       true -> ok
    end,
    SR = lists:map(fun(V) -> V/Iter end, lists:sublist(lists:sort(Results), round(Max*0.75))),
    Avg = lists:sum(SR)/length(SR),
    F = fun(X, Sum) -> Sum + (X - Avg) * (X - Avg) end,
    Var = lists:foldl(F, 0.0, SR) / length(SR),
    {Avg, math:sqrt(Var)};
measure_helper(Fun, Attrs, Iter, Results, Counter, Max, Print) ->
    if Print -> io:format("\rMeasuring (~B/~B)      ", [Counter, Max]);
       true -> ok
    end,
    {Time, _Res} = timer:tc(fun loop/3, [Fun, Attrs, Iter]),
    measure_helper(Fun, Attrs, Iter, [Time|Results], Counter+1, Max, Print).

-spec loop(fun(), list(), number()) -> ok.
loop(_, _, 0) ->
    ok;
loop(Fun, Attrs, Count) ->
    apply(Fun, Attrs),
    loop(Fun, Attrs, Count-1).
