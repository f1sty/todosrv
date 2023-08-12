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

handle_call({user_todos, User}, From, #{conn := C} = State) ->
  {ok, Cols, Rows} = epgsql:squery(C, "select * from users"),
  FieldNames = field_names(Cols),
  List = lists:map(fun(Entry) -> maps:from_list(lists:zip(FieldNames, tuple_to_list(Entry))) end, Rows),
  {reply, List, State}.

user_todos(User) ->
  gen_server:call(?MODULE, {user_todos, User}).

field_names(Cols) ->
  [FieldName || {column, FieldName, _, _, _, _, _, _, _} <- Cols].
