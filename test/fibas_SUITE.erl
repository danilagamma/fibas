-module(fibas_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        number_api,
        list_api,
        blacklist_api
    ].

init_per_suite(Config) ->
    application:load(fibas),
    DetsFile = "blacklist_db.ct_" ++ atom_to_list(node()) ++ ".dets",
    ct:pal("Placing blacklist_db at ~s", [DetsFile]),
    application:set_env(fibas, blacklist_db_filename, DetsFile),
    {ok, _} = application:ensure_all_started(fibas),
    {ok, _} = application:ensure_all_started(inets),
    {ok, Port} = application:get_env(fibas, http_port),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    [{url, Url}, {dets_file, DetsFile}|Config].

end_per_suite(Config) ->
    application:stop(fibas),
    DetsFile = filename:join(code:priv_dir(fibas), ?config(dets_file, Config)),
    ct:pal("Deleting ~s: ~p", [DetsFile, file:delete(DetsFile)]),
    Config.

number_api(Config) ->
    ?assertEqual(
        {200, #{<<"index">> => 10, <<"fibonacci">> => 55}},
        http_request(get, url("/fib/~p", [10], Config))
    ),
    ?assertEqual(
        {400, #{
            <<"status">> => <<"error">>,
            <<"reason">> =>
                <<"Invalid parameter 'index', must be between 1 and 100000, got: 0">>
        }},
        http_request(get, url("/fib/~p", [0], Config))
    ),
    ?assertEqual(
        {400, #{
            <<"status">> => <<"error">>,
            <<"reason">> =>
                <<"Invalid parameter 'index', must be between 1 and 100000, got: 100001">>
        }},
        http_request(get, url("/fib/~p", [100001], Config))
    ).

list_api(Config) ->
    ?assertEqual(
        {200, #{
            <<"list">> =>
                [
                    #{<<"index">> => 1, <<"fibonacci">> => 1},
                    #{<<"index">> => 2, <<"fibonacci">> => 1},
                    #{<<"index">> => 3, <<"fibonacci">> => 2}
                ],
            <<"next_start_index">> => 4
        }},
        http_request(get, url("/fib/~p/list?page_size=~p", [10, 3], Config))
    ),
    ?assertEqual(
        {200, #{
            <<"list">> =>
            [
                #{<<"index">> => 8,  <<"fibonacci">> => 21},
                #{<<"index">> => 9,  <<"fibonacci">> => 34},
                #{<<"index">> => 10, <<"fibonacci">> => 55}
            ]
        }},
        http_request(get, url("/fib/~p/list?page_size=~p&start_index=~p", [10, 3, 8], Config))
    ),
    ?assertEqual(
        {400, #{
            <<"status">> => <<"error">>,
            <<"reason">> => <<"Invalid parameter 'page_size', must be between 1 and 100, got: 101">>
        }},
        http_request(get, url("/fib/~p/list?page_size=~p", [10, 101], Config))
    ).

blacklist_api(Config) ->
    ?assertEqual(
        {200, #{
            <<"list">> =>
            [
                #{<<"index">> => 1, <<"fibonacci">> => 1},
                #{<<"index">> => 2, <<"fibonacci">> => 1},
                #{<<"index">> => 3, <<"fibonacci">> => 2}
            ]
        }},
        http_request(get, url("/fib/~p/list?page_size=~p", [3, 3], Config))
    ),
    [ ?assertEqual(
        {204, <<>>},
        http_request(put, url("/fib/~p/blacklist", [N], Config))
      ) || N <- [1, 2] ],
    ?assertEqual(
        {200, #{
            <<"list">> =>
            [
                #{<<"index">> => 3, <<"fibonacci">> => 2}
            ]
        }},
        http_request(get, url("/fib/~p/list?page_size=~p", [3, 3], Config))
    ),
    ?assertEqual(
        {200, #{<<"index">> => 1, <<"fibonacci">> => <<"blacklisted">>}},
        http_request(get, url("/fib/~p", [1], Config))
    ),
    ?assertEqual(
        {204, <<>>},
        http_request(delete, url("/fib/~p/blacklist", [1], Config))
    ),
    ?assertEqual(
        {200, #{<<"index">> => 1, <<"fibonacci">> => 1}},
        http_request(get, url("/fib/~p", [1], Config))
    ),
    ?assertEqual(
        {200, #{
            <<"list">> =>
            [
                #{<<"index">> => 1, <<"fibonacci">> => 1},
                #{<<"index">> => 3, <<"fibonacci">> => 2}
            ]
        }},
        http_request(get, url("/fib/~p/list?page_size=~p", [3, 3], Config))
    ).

url(Format, Args, Config) ->
    Path = iolist_to_binary([?config(url, Config), io_lib:format(Format, Args)]),
    binary_to_list(Path).

http_request(Method, Url) ->
    case httpc:request(Method, {Url, []}, [], []) of
        {ok, {{_, StatusCode, _}, _Headers, Response}} ->
            Data = case Response of
                       []       -> <<>>;
                       Response -> jiffy:decode(Response, [return_maps])
                   end,
            {StatusCode, Data};
        Other ->
            Other
    end.