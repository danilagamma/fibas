-module(fibas_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

-define(APP, fibas).
-define(REST_HANDLER, fibas_rest_handler).

start(_StartType, _StartArgs) ->
    ok = fibas_memoize:bootstrap(),
    ok = fibas_blacklist_db:bootstrap(),
    Port = application:get_env(?APP, http_port, 8080),
    Dispatch = cowboy_router:compile(fibas_rest_handler:routes()),
    _ = cowboy:start_clear(http_listener, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    fibas_sup:start_link().

stop(_State) ->
    ok = fibas_blacklist_db:teardown(),
    ok.
