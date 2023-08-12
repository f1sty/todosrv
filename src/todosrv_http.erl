-module(todosrv_http).
-export([init/2, allowed_methods/2, content_types_provided/2, todos_handler/2,
         content_types_accepted/2]).
-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
  ?LOG_INFO("~p", [Req]),
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, todos_handler}], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, todos_handler}], Req, State}.

todos_handler(#{method := <<"GET">>, bindings := #{user := User}} = Req, State) ->
  Todos = todosrv_db:user_todos(User),
  Body = #{todos => Todos},
  {jiffy:encode(Body), Req, State};
todos_handler(#{method := <<"POST">>, bindings := #{user := User}} = Req, State) ->
  BodyRaw = #{todos => [#{user => User, title => <<"feed the cat">>, status => done}]},
  Body = jiffy:encode(BodyRaw),
  {Body, Req, State};
todos_handler(#{method := <<"PATCH">>, bindings := #{user := User}} = Req, State) ->
  BodyRaw = #{todos => [#{user => User, title => <<"feed the cat">>, status => done}]},
  Body = jiffy:encode(BodyRaw),
  {Body, Req, State};
todos_handler(Req, State) ->
  BodyRaw = #{todos => [#{user => none, title => <<"feed the cat">>, status => done}]},
  Body = jiffy:encode(BodyRaw),
  {Body, Req, State}.
