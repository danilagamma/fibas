-module(fibas_rest_handler).

-export([
    routes/0,
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    handle_request/2
]).

%% Index limit for fibonacci number
-define(INDEX_LIMIT,       100000).
-define(DEFAULT_PAGE_SIZE, 100).
-define(PAGE_SIZE_LIMIT,   100).

routes() ->
    [
        {'_', [
            {"/fib/:index",      ?MODULE, #{api => number}},
            {"/fib/:index/list", ?MODULE, #{api => list}}
        ]}
    ].

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_request}], Req, State}.

handle_request(Req, State) ->
    Fun = case cowboy_req:method(Req) of
              <<"GET">> -> fun get/2
          end,
    try Fun(Req, State) of
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