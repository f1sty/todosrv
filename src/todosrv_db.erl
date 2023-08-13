-module(todosrv_db).

-export([start_link/1, init/1, handle_call/3, list_todos_by_user_id/1,
         delete_todo_by_user_id_todo_id/2, create_todo_by_user_id/2, update_todo_by_user_id/3]).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

start_link(Config) ->
    {ok, C} = epgsql:connect(maps:from_list(Config)),
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
    {reply, Ret, State};
handle_call({create_todo_by_user_id, UserId, TodoContent}, _From, #{conn := C} = State) ->
    {ok, 1, Cols, Rows} =
        epgsql:equery(C,
                      "INSERT INTO todos(user_id, content) VALUES ($1, $2) RETURNING id",
                      [UserId, TodoContent]),
    Uri = io_lib:format("/~b/todos", [UserId]),
    {reply, Uri, State};
handle_call({update_todo_by_user_id, UserId, TodoId, IsDone},
            _From,
            #{conn := C} = State) ->
    {ok, 1} =
        epgsql:equery(C,
                      "UPDATE todos SET done = $1 WHERE user_id = $2 AND id = $3",
                      [IsDone, UserId, TodoId]),
    {reply, ok, State}.

list_todos_by_user_id(UserId) ->
    gen_server:call(?MODULE, {list_todos_by_user_id, UserId}).

delete_todo_by_user_id_todo_id(UserId, TodoId) ->
    gen_server:call(?MODULE, {delete_todo_by_user_id_todo_id, UserId, TodoId}).

create_todo_by_user_id(UserId, Todo) ->
    gen_server:call(?MODULE, {create_todo_by_user_id, UserId, Todo}).

update_todo_by_user_id(UserId, TodoId, IsDone) ->
    gen_server:call(?MODULE, {update_todo_by_user_id, UserId, TodoId, IsDone}).

field_names(Cols) ->
    [FieldName || {column, FieldName, _, _, _, _, _, _, _} <- Cols].

map(Keys, Values) ->
    maps:from_list(
        lists:zip(Keys, tuple_to_list(Values))).
