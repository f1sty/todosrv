%%%-------------------------------------------------------------------
%% @doc todosrv public API
%% @end
%%%-------------------------------------------------------------------

-module(todosrv_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(epgsql),

    logger:set_module_level(todosrv_controller, info),
    logger:set_module_level(todosrv_db, info),

    Routes = [{"/[:user]/todos/[:todo]", todosrv_controller, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),

    {ok, _} = cowboy:start_clear(http, [{port, 9090}], #{env => #{dispatch => Dispatch}}),

    todosrv_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    ok.

%% internal functions
