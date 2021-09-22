-module(fibas_rest_handler).

-export([
    routes/0,
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    delete_resource/2,
    handle_request/2
]).

%% Index limit for fibonacci number
-define(INDEX_LIMIT,       100000).
-define(DEFAULT_PAGE_SIZE, 100).
-define(PAGE_SIZE_LIMIT,   100).

-spec routes() ->
    [cowboy_router:route()].
routes() ->
    [
        {'_', [
            {"/fib/:index",           ?MODULE, #{api => number}},
            {"/fib/:index/list",      ?MODULE, #{api => list}},
            {"/fib/:index/blacklist", ?MODULE, #{api => blacklist}}
        ]}
    ].

-spec init(cowboy_req:req(), map()) ->
    {cowboy_rest, cowboy_req:req(), map()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(cowboy_req:req(), map()) ->
    {[cowboy:method()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), map()) ->
    {[{atom(), atom()}], cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
    {[{'*', handle_request}], Req, State}.

-spec content_types_provided(cowboy_req:req(), map()) ->
    {[{binary(), atom()}], cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request}], Req, State}.

-spec delete_resource(cowboy_req:req(), map()) ->
    {true | iodata(), cowboy_req:req(), map()}.
delete_resource(Req, State) ->
    handle_request(Req, State).

-spec handle_request(cowboy_req:req(), map()) ->
    {true | iodata() | stop, cowboy_req:req(), map()}.
handle_request(Req, State) ->
    Fun = case cowboy_req:method(Req) of
              <<"GET">>    -> fun get/2;
              <<"PUT">>    -> fun put/2;
              <<"DELETE">> -> fun delete/2
          end,
    try Fun(Req, State) of
        ok ->
            {true, Req, State};
        {ok, Result} ->
            {jiffy:encode(Result), Req, State};
        {error, StatusCode, Reason} ->
            error_reply(StatusCode, Reason, Req),
            {stop, Req, State}
    catch
        throw:Reason ->
            error_reply(400, iolist_to_binary(format_error_reason(Reason)), Req),
            {stop, Req, State}
    end.

get(Req, #{api := number}) ->
    Index = get_index_binding(Req),
    {ok, fibas_api:number(Index)};
get(Req, #{api := list}) ->
    QsVals     = cowboy_req:parse_qs(Req),
    StartIndex = get_qs_val(<<"start_index">>, [fun is_integer/1, fun is_valid_index/1],
                            1, QsVals),
    PageSize   = get_qs_val(<<"page_size">>, [fun is_integer/1, fun is_valid_page_size/1],
                            ?DEFAULT_PAGE_SIZE, QsVals),
    EndIndex   = get_index_binding(Req),
    {ok, fibas_api:list(StartIndex, EndIndex, PageSize)};
get(_Req, _State) ->
    {error, 404, <<"path_not_found">>}.

put(Req, #{api := blacklist}) ->
    Index = get_index_binding(Req),
    ok = fibas_api:blacklist_index(Index);
put(_Req, _State) ->
    {error, 404, <<"path_not_found">>}.

delete(Req, #{api := blacklist}) ->
    Index = get_index_binding(Req),
    ok = fibas_api:whitelist_index(Index);
delete(_Req, _State) ->
    {error, 404, <<"path_not_found">>}.

get_index_binding(Req) ->
    get_binding(index, [fun is_integer/1, fun is_valid_index/1], Req).

get_binding(Name, ValidationFuns, Req) ->
    Value = cowboy_req:binding(Name, Req),
    validate(Value, ValidationFuns, Name).

get_qs_val(Name, ValidationFuns, Default, Req) ->
    case proplists:get_value(Name, Req) of
        undefined ->
            Default;
        Value ->
            validate(Value, ValidationFuns, Name)
    end.

validate(Value, [], _Name) ->
    Value;
validate(Value, [ValidationFun|RestFuns], Name) ->
    try ValidationFun(Value) of
        {true, NewValue} ->
            validate(NewValue, RestFuns, Name);
        true ->
            validate(Value, RestFuns, Name);
        {false, Reason} ->
            throw({invalid_value, Name, Value, Reason})
    catch
        _Any:_Reason ->
            throw({invalid_value, Name, Value})
    end.

is_integer(Value) ->
    try
        {true, binary_to_integer(Value)}
    catch
        _:_ ->
            {false, "must be an integer"}
    end.

is_valid_page_size(Value)
  when Value >= 1, Value =< ?PAGE_SIZE_LIMIT ->
    true;
is_valid_page_size(_) ->
    {false, io_lib:format("must be between ~p and ~p", [1, ?PAGE_SIZE_LIMIT])}.

is_valid_index(Value)
  when Value >= 1, Value =< ?INDEX_LIMIT ->
    true;
is_valid_index(_) ->
    {false, io_lib:format("must be between ~p and ~p", [1, ?INDEX_LIMIT])}.

format_error_reason({invalid_value, Name, Value, Reason}) ->
    io_lib:format("Invalid parameter '~s', ~s, got: ~p", [Name, Reason, Value]);
format_error_reason({invalid_value, Name, Value}) ->
    io_lib:format("Invalid parameter '~s, got: ~p", [Name, Value]).

error_reply(Status, Reason, Req) ->
    Message = jiffy:encode(#{status => error,
                             reason => Reason}),
    cowboy_req:reply(Status, #{}, Message, Req).