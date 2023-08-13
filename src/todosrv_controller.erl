-module(todosrv_controller).

-export([init/2, allowed_methods/2, content_types_provided/2, todos_handler/2,
         content_types_accepted/2, delete_resource/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, todos_handler}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, todos_handler}], Req, State}.

todos_handler(#{method := <<"GET">>, bindings := #{user := User}} = Req, State) ->
    try binary_to_integer(User) of
        UserId ->
            Todos = todosrv_db:list_todos_by_user_id(UserId),
            Body = #{todos => Todos},
            {jiffy:encode(Body), Req, State}
    catch
        error:badarg ->
            cowboy_req:reply(400, #{}, Req),
            {stop, Req, State}
    end;
todos_handler(#{method := <<"POST">>, bindings := #{user := User}} = Req0, State) ->
    {ok, [{BodyRaw, true}], Req} = cowboy_req:read_urlencoded_body(Req0),
    {Body} = jiffy:decode(BodyRaw),
    {<<"content">>, TodoContent} = lists:keyfind(<<"content">>, 1, Body),
    try binary_to_integer(User) of
        UserId ->
            Uri = todosrv_db:create_todo_by_user_id(UserId, TodoContent),
            {{created, Uri}, Req, State}
    catch
        error:badarg ->
            cowboy_req:reply(400, #{}, Req),
            {stop, Req, State}
    end;
todos_handler(#{method := <<"PATCH">>, bindings := #{user := User, todo := Todo}} = Req0,
              State) ->
    {ok, [{BodyRaw, true}], Req} = cowboy_req:read_urlencoded_body(Req0),
    {Body} = jiffy:decode(BodyRaw),
    {<<"done">>, IsDone} = lists:keyfind(<<"done">>, 1, Body),
    try {binary_to_integer(User), binary_to_integer(Todo)} of
        {UserId, TodoId} ->
            todosrv_db:update_todo_by_user_id(UserId, TodoId, IsDone),
            {true, Req, State}
    catch
        error:badarg ->
            cowboy_req:reply(400, #{}, Req),
            {stop, Req, State}
    end.

delete_resource(#{bindings := #{user := User, todo := Todo}} = Req, State) ->
    try {binary_to_integer(User), binary_to_integer(Todo)} of
        {UserId, TodoId} ->
            {ok, _} = todosrv_db:delete_todo_by_user_id_todo_id(UserId, TodoId),
            {true, Req, State}
    catch
        error:badarg ->
            {false, Req, State}
    end.
