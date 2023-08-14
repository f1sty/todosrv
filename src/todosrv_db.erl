-module(todosrv_db).

% Callbacks
-export([init/1, handle_call/3, handle_cast/2]).
% APIs
-export([start_link/1, get_user_todos/1, get_user_todo/2, get_user_id_verified/2,
         delete_user_todo/2, create_user_todo/2, update_user_todo/3]).

-behaviour(gen_server).

% Callbacks

init(Args) ->
    {ok, Args}.

% TODO: handle properly, vulnerable to SQL-injection
handle_call({get_user_todos, UserId}, _From, #{conn := C} = State) ->
    {ok, Cols, Rows} =
        epgsql:equery(C, "SELECT id, text, done FROM todos WHERE user_id = $1", [UserId]),
    KList = keyword_list(Cols, Rows),
    {reply, KList, State};
handle_call({get_user_todo, UserId, TodoId}, _From, #{conn := C} = State) ->
    {ok, Cols, Rows} =
        epgsql:equery(C,
                      "SELECT id, text, done FROM todos WHERE user_id = $1 AND id = $2 LIMIT 1",
                      [UserId, TodoId]),
    KList = keyword_list(Cols, Rows),
    {reply, KList, State};
handle_call({delete_user_todo, UserId, TodoId}, _From, #{conn := C} = State) ->
    {ok, Count} =
        epgsql:equery(C, "DELETE FROM todos WHERE user_id = $1 AND id = $2", [UserId, TodoId]),
    {reply, Count, State};
handle_call({create_user_todo, UserId, TodoText}, _From, #{conn := C} = State) ->
    {ok, 1, _Cols, [{TodoId}]} =
        epgsql:equery(C,
                      "INSERT INTO todos(user_id, text) VALUES ($1, $2) RETURNING id",
                      [UserId, TodoText]),
    {reply, TodoId, State};
handle_call({update_user_todo, UserId, TodoId, IsDone}, _From, #{conn := C} = State) ->
    {ok, Count} =
        epgsql:equery(C,
                      "UPDATE todos SET done = $1 WHERE user_id = $2 AND id = $3",
                      [IsDone, UserId, TodoId]),
    {reply, Count, State};
handle_call({get_user_id_verified, UserId, Password}, _From, #{conn := C} = State) ->
    case epgsql:equery(C,
                       "SELECT encrypted_password = crypt($1, encrypted_password) AS is_correct, id FROM users WHERE id = $2",
                       [Password, UserId])
    of
        {ok, _Cols, [{true, UserId}]} ->
            {reply, {ok, UserId}, State};
        _ ->
            {reply, {error, <<"wrong credentials">>}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

% APIs

start_link(Config) ->
    {ok, C} =
        epgsql:connect(
            maps:from_list(Config)),
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{conn => C}, []).

get_user_todos(UserId) ->
    gen_server:call(?MODULE, {get_user_todos, UserId}).

get_user_todo(UserId, TodoId) ->
    gen_server:call(?MODULE, {get_user_todo, UserId, TodoId}).

delete_user_todo(UserId, TodoId) ->
    gen_server:call(?MODULE, {delete_user_todo, UserId, TodoId}).

create_user_todo(UserId, Todo) ->
    gen_server:call(?MODULE, {create_user_todo, UserId, Todo}).

update_user_todo(UserId, TodoId, IsDone) ->
    gen_server:call(?MODULE, {update_user_todo, UserId, TodoId, IsDone}).

get_user_id_verified(UserId, Password) ->
    gen_server:call(?MODULE, {get_user_id_verified, UserId, Password}).

% Privat functions

keyword_list(Cols, Rows) ->
    Keys = keys(Cols),
    lists:map(fun(Values) -> tuple(Keys, Values) end, Rows).

keys(Cols) ->
    [FieldName || {column, FieldName, _, _, _, _, _, _, _} <- Cols].

tuple(Keys, Values) ->
    maps:from_list(
        lists:zip(Keys, tuple_to_list(Values))).
