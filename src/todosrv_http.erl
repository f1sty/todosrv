-module(todosrv_http).
-export([init/2, allowed_methods/2, content_types_provided/2, todos_handler/2]).
-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
  ?LOG_INFO("~p", [Req]),
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, todos_handler}], Req, State}.


todos_handler(Req, State) ->
  BodyRaw = #{todos => [#{title => <<"feed the cat">>, status => done}]},
  Body = jiffy:encode(BodyRaw),
  % Body = <<"{\"rest\": 123 }">>,
  {Body, Req, State}.
