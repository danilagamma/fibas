-module(fibas_memoize).

%% API
-export([
    bootstrap/0,
    apply/2
]).

-define(MEMO_TAB, ?MODULE).

-spec bootstrap() -> ok.
bootstrap() ->
    _ = ets:new(?MEMO_TAB, [named_table, set, public]),
    ok.

-spec apply(function(), [any()]) -> any().
apply(Fun, Args) ->
    Key = {Fun, Args},
    case ets:lookup(?MEMO_TAB, Key) of
        [] ->
            Result = erlang:apply(Fun, Args),
            ets:insert(?MEMO_TAB, {Key, Result}),
            Result;
        [{_, Result}] ->
            Result
    end.
