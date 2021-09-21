-module(fibas_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        number_api,
        list_api
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(fibas),
    {ok, _} = application:ensure_all_started(inets),
    {ok, Port} = application:get_env(fibas, http_port),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    [{url, Url}|Config].

end_per_suite(Config) ->
    Config.

number_api(Config) ->
    ?assertEqual(
        {200, #{<<"index">> => 10, <<"fibonacci">> => 55}},
        http_get(get, url("/fib/~p", [10], Config))
    ),
    ?assertEqual(
        {400, #{
            <<"status">> => <<"error">>,
            <<"reason">> =>
                <<"Invalid parameter 'index', must be between 1 and 100000, got: 0">>
        }},
        http_get(get, url("/fib/~p", [0], Config))
    ),
    ?assertEqual(
        {400, #{
            <<"status">> => <<"error">>,
            <<"reason">> =>
                <<"Invalid parameter 'index', must be between 1 and 100000, got: 100001">>
        }},
        http_get(get, url("/fib/~p", [100001], Config))
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
        http_get(get, url("/fib/~p/list?page_size=~p", [10, 3], Config))
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
        http_get(get, url("/fib/~p/list?page_size=~p&start_index=~p", [10, 3, 8], Config))
    ),
    ?assertEqual(
        {400, #{
            <<"status">> => <<"error">>,
            <<"reason">> => <<"Invalid parameter 'page_size', must be between 1 and 100, got: 101">>
        }},
        http_get(get, url("/fib/~p/list?page_size=~p", [10, 101], Config))
    ).

url(Format, Args, Config) ->
    Path = iolist_to_binary([?config(url, Config), io_lib:format(Format, Args)]),
    binary_to_list(Path).

http_get(Method, Url) ->
    case httpc:request(Method, {Url, []}, [], []) of
        {ok, {{_, StatusCode, _}, _Headers, Response}} ->
            {StatusCode, jiffy:decode(Response, [return_maps])};
        Other ->
            Other
    end.