-module(todosrv_controller).

% Callbacks
-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2,
         delete_resource/2, is_authorized/2]).
% ProvideCallback
-export([to_json/2]).
% AcceptCallback
-export([from_json/2]).

% Callbacks

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

% TODO: handle malformed tokens
% TODO: use hashed passwords in token
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {bearer, Token} ->
            try binary_to_term(base64:decode(Token)) of
                {TokenUserId, Password} ->
                    case current_user(Req) of
                        {TokenUserId, Req1} ->
                            {true, Req1, State};
                        {_, Req1} ->
                            login(TokenUserId, Password, Req1, State)
                    end
            catch
                error:_ ->
                    {{false, <<"malformed token">>}, Req, State}
            end;
        _ ->
            {{false, <<"malformed token">>}, Req, State}
    end.

% ProvideCallback

to_json(#{method := <<"GET">>, bindings := #{todo_id := TodoId}} = Req, State) ->
    {UserId, Req0} = current_user(Req),
    logger:set_primary_config(level, info),
    case todosrv_db:get_user_todo(UserId, TodoId) of
        [] ->
            {"", Req0, State};
        [Body] ->
            {jiffy:encode(Body), Req0, State}
    end;
to_json(#{method := <<"GET">>} = Req, State) ->
    {UserId, Req0} = current_user(Req),
    Todos = todosrv_db:get_user_todos(UserId),
    Body = #{todos => Todos},

    {jiffy:encode(Body), Req0, State};
to_json(Req, State) ->
    {true, Req, State}.

% AcceptCallback

from_json(#{method := <<"POST">>} = Req, State) ->
    {UserId, Req0} = current_user(Req),
    {Req1, TodoText} = get_body_value(Req0, <<"text">>),
    TodoId = todosrv_db:create_user_todo(UserId, TodoText),
    Uri = io_lib:format("/todos/~s", [TodoId]),

    {{created, Uri}, Req1, State};
from_json(#{method := <<"PATCH">>, bindings := #{todo_id := TodoId}} = Req, State) ->
    {UserId, Req0} = current_user(Req),
    {Req1, IsDone} = get_body_value(Req0, <<"done">>),
    _Count = todosrv_db:update_user_todo(UserId, TodoId, IsDone),

    {true, Req1, State};
from_json(Req, State) ->
    {true, Req, State}.

delete_resource(#{bindings := #{todo_id := TodoId}} = Req, State) ->
    {UserId, Req0} = current_user(Req),
    case todosrv_db:delete_user_todo(UserId, TodoId) of
        0 ->
            {false, Req0, State};
        1 ->
            {true, Req0, State}
    end.

% Private functions

get_body_value(Req0, Key) ->
    {ok, [{BodyRaw, true}], Req} = cowboy_req:read_urlencoded_body(Req0),
    {Body} = jiffy:decode(BodyRaw),
    {Key, Value} = lists:keyfind(Key, 1, Body),

    {Req, Value}.

login(UserId, Password, #{path := Path} = Req, State) ->
    case todosrv_db:get_user_id_verified(UserId, Password) of
        {ok, TokenUserId} ->
            {ok, Req1} = cowboy_session:set(user_id, TokenUserId, Req),
            Req2 = cowboy_req:reply(307, #{<<"location">> => Path}, Req1),

            {stop, Req2, State};
        {error, Reason} ->
            {{false, Reason}, Req, State}
    end.

current_user(Req) ->
    cowboy_session:get(user_id, Req).
