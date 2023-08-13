FROM erlang:26-alpine as builder

RUN mkdir /data
WORKDIR /data

RUN apk add --no-cache gcc g++

COPY src src/
COPY rebar.config .
RUN rebar3 release

FROM alpine

RUN apk add --no-cache openssl ncurses-libs libstdc++ libgcc

COPY --from=builder /data/_build/default/rel/todosrv /todosrv

EXPOSE 9090
EXPOSE 8443

ENTRYPOINT ["/todosrv/bin/todosrv"]
CMD ["foreground"]
