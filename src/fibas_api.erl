-module(fibas_api).

%% API
-export([
    number/1,
    list/3
]).

-spec number(pos_integer()) -> map().
number(Index) ->
    #{index     => Index,
      fibonacci => fibas_fib:calculate(Index)}.

-spec list(pos_integer(), pos_integer(), pos_integer()) -> map().
list(StartIndex, EndIndex, PageSize) ->
    LastIndex = StartIndex + PageSize - 1,
    Sequence  = lists:seq(StartIndex, min(LastIndex, EndIndex)),
    Result    = #{list => [ number(N) || N <- Sequence ]},
    case LastIndex < EndIndex of
        true  -> Result#{next_start_index => LastIndex + 1};
        false -> Result
    end.