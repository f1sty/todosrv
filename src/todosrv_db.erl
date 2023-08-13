-module(todosrv_db).
-export([start_link/1, init/1, handle_call/3, user_todos/1]).
-behaviour(gen_server).

start_link(_Args) ->
  {ok, _} = application:ensure_all_started(epgsql),
  {ok, C} = epgsql:connect(#{
                             host => "database",
                             database => "todosrv_db",
                             username => "todosrv",
                             password => "todosrv_supersecure"
                            }),
  gen_server:start_link({local, ?MODULE}, ?MODULE, #{conn => C}, []).

init(Args) ->
  {ok, Args}.

handle_call({user_todos, UserId}, From, #{conn := C} = State) ->
  {ok, Cols, Rows} = epgsql:equery(C, "select t.id, content, done from todos as t join users as u on
                                   user_id = u.id where user_id = $1", [UserId]),
  Keys = field_names(Cols),
  List = lists:map(fun(Values) -> map(Keys, Values) end, Rows),
  {reply, List, State}.

user_todos(UserId) ->
  gen_server:call(?MODULE, {user_todos, UserId}).

field_names(Cols) ->
  [FieldName || {column, FieldName, _, _, _, _, _, _, _} <- Cols].

map(Keys, Values) ->
  maps:from_list(lists:zip(Keys, tuple_to_list(Values))).
