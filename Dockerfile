FROM erlang:26-alpine

RUN mkdir /data
WORKDIR /data

COPY src src/
COPY rebar.config .
RUN rebar3 release

FROM alpine

RUN apk add --no-cache openssl ncurses-libs libstdc++ libgcc

COPY --from=0 /data/_build/default/rel/todosrv /todosrv

EXPOSE 9090
EXPOSE 8443

CMD ["/todosrv/bin/todosrv", "foreground"]
