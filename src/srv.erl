-module(srv).
-author("Ricardo Cosme <ricjcosme@gmail.com>").
-compile(export_all).
-define(PORT, 10001).
-define(SERVERS, 10).
-define(TIMEOUT, 60000).
-define(PACKET, line).
-define(BINARY_OR_LIST, binary).

start() ->
    case gen_tcp:listen(?PORT,[{active, false},{packet,?PACKET},?BINARY_OR_LIST]) of
        {ok, ListenSock} ->
            start_servers(?SERVERS,ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error,Reason} ->
            {error,Reason}
    end.

start_servers(0,_) ->
    ok;
start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1,LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        _ ->
            ok
    end.

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
            {ok,{RemoteAddr,_}} = inet:peername(S),
            work_data(Data,RemoteAddr),
            loop(S);
        {tcp_closed,S} ->
            ok
    after ?TIMEOUT ->
        gen_tcp:close(S)
    end.

work_data(Data,_) when length(Data) < 1 ->
    do_nothing;
work_data(Data,RemoteAddr) ->
    work_final(Data,RemoteAddr).

work_final(Data,RemoteAddr) ->
    io:format("~p~n", [{Data,inet_parse:ntoa(RemoteAddr)}]).


