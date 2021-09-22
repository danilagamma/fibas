-module(fibas_blacklist_db).

-export([
    bootstrap/0,
    teardown/0,
    member/1,
    insert/1,
    delete/1,
    list/2
]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(DETS_TAB, fibas_blacklist_table).
-define(ETS_TAB, fibas_blacklist_table_ets).

-spec bootstrap() -> ok.
bootstrap() ->
    Filename = application:get_env(fibas, blacklist_db_filename, "blacklist_db.dets"),
    FilePath = filename:join(code:priv_dir(fibas), Filename),
    {ok, ?DETS_TAB} = dets:open_file(?DETS_TAB, [
        {type, set},
        {auto_save, timer:seconds(30)},
        {file, FilePath}
    ]),
    ?ETS_TAB = ets:new(?ETS_TAB, [
        named_table,
        public,
        ordered_set,
        {read_concurrency, true}
    ]),
    ?ETS_TAB = dets:to_ets(?DETS_TAB, ?ETS_TAB),
    ok.

-spec teardown() -> ok.
teardown() ->
    ok = dets:close(?DETS_TAB).

-spec member(pos_integer()) -> boolean().
member(Index) ->
    case ets:lookup(?ETS_TAB, Index) of
        [{Index}] -> true;
        []        -> false
    end.

-spec insert(pos_integer()) -> ok.
insert(Index) ->
    ok = dets:insert(?DETS_TAB, {Index}),
    true = ets:insert(?ETS_TAB, {Index}),
    ok.

-spec delete(pos_integer()) -> ok.
delete(Index) ->
    ok = dets:delete(?DETS_TAB, Index),
    true = ets:delete(?ETS_TAB, Index),
    ok.

-spec list(pos_integer(), pos_integer()) -> [pos_integer()].
list(From, To) when From =< To ->
    ets:select(?ETS_TAB, ets:fun2ms(fun({K}) when K >= From, K =< To -> K end)).
