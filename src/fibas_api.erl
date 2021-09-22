-module(fibas_api).

%% API
-export([
    number/1,
    list/3,
    blacklist_index/1,
    whitelist_index/1
]).

-spec number(pos_integer()) -> map().
number(Index) ->
    Value = case fibas_blacklist_db:member(Index) of
                true  -> blacklisted;
                false -> fibas_fib:calculate(Index)
            end,
    #{index     => Index,
      fibonacci => Value}.

-spec list(pos_integer(), pos_integer(), pos_integer()) -> map().
list(StartIndex, EndIndex, PageSize) ->
    LastIndex = StartIndex + PageSize - 1,
    LastSeq   = min(LastIndex, EndIndex),
    Sequence  = lists:seq(StartIndex, LastSeq),
    Blacklist = fibas_blacklist_db:list(StartIndex, LastSeq),
    Result    = #{list => [ number(N) || N <- ordsets:subtract(Sequence, Blacklist) ]},
    case LastIndex < EndIndex of
        true  -> Result#{next_start_index => LastIndex + 1};
        false -> Result
    end.

-spec blacklist_index(pos_integer()) -> ok.
blacklist_index(Index) ->
    ok = fibas_blacklist_db:insert(Index).

-spec whitelist_index(pos_integer()) -> ok.
whitelist_index(Index) ->
    ok = fibas_blacklist_db:delete(Index).