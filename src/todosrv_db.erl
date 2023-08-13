-module(todosrv_db).

-export([start_link/1, init/1, handle_call/3, list_todos_by_user_id/1,
         delete_todo_by_user_id_todo_id/2]).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

start_link(_Args) ->
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, C} =
        epgsql:connect(#{host => "database",
                         database => "todosrv_db",
                         username => "todosrv",
                         password => "todosrv_supersecure"}),
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{conn => C}, []).

init(Args) ->
    {ok, Args}.

handle_call({list_todos_by_user_id, UserId}, _From, #{conn := C} = State) ->
    {ok, Cols, Rows} =
        epgsql:equery(C,
                      "SELECT t.id, content, done FROM todos AS t JOIN users AS u ON
                                   user_id = u.id WHERE user_id = $1",
                      [UserId]),
    Keys = field_names(Cols),
    List = lists:map(fun(Values) -> map(Keys, Values) end, Rows),
    {reply, List, State};
handle_call({delete_todo_by_user_id_todo_id, UserId, TodoId},
            _From,
            #{conn := C} = State) ->
    Ret = epgsql:equery(C,
                        "DELETE FROM todos WHERE user_id = $1 AND id = $2",
                        [UserId, TodoId]),
    {reply, Ret, State}.

list_todos_by_user_id(UserId) ->
    gen_server:call(?MODULE, {list_todos_by_user_id, UserId}).

delete_todo_by_user_id_todo_id(UserId, TodoId) ->
    gen_server:call(?MODULE, {delete_todo_by_user_id_todo_id, UserId, TodoId}).

field_names(Cols) ->
    [FieldName || {column, FieldName, _, _, _, _, _, _, _} <- Cols].

map(Keys, Values) ->
    maps:from_list(
        lists:zip(Keys, tuple_to_list(Values))).
