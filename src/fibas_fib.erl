-module(fibas_fib).

%% API
-export([
    calculate/1
]).

-spec calculate(pos_integer()) -> pos_integer().
calculate(1) ->
    1;
calculate(2) ->
    1;
calculate(N) when is_integer(N), N > 2 ->
    fibas_memoize:apply(fun do_calculate/1, [N]).

do_calculate(N) ->
    calculate(N - 2) + calculate(N - 1).
