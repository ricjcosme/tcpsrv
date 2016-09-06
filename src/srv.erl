-module(srv).
-author("Ricardo Cosme <ricjcosme@gmail.com>").
-compile(export_all).
-define(PORT, case os:getenv("TCPSRV_PORT") of false -> 10001; _ -> os:getenv("TCPSRV_PORT") end).
-define(SERVERS, case os:getenv("TCPSRV_SERVERS") of false -> 10; _ -> os:getenv("TCPSRV_SERVERS") end).
-define(TIMEOUT, case os:getenv("TCPSRV_TIMEOUT") of false -> 60000; _ -> os:getenv("TCPSRV_TIMEOUT") end).
-define(PACKET, case os:getenv("TCPSRV_PACKET") of false -> line; _ -> os:getenv("TCPSRV_PACKET") end).
-define(BINARY_OR_LIST, case os:getenv("TCPSRV_BINARY_OR_LIST") of false -> binary; _ -> os:getenv("TCPSRV_BINARY_OR_LIST") end).
%% DATASTORE redis | elasticsearch (default) | solr, others can be added via toDataStore/2
-define(DATASTORE, case os:getenv("TCPSRV_DATASTORE") of false -> elasticsearch; _ -> os:getenv("TCPSRV_DATASTORE") end).
-define(DST_HOST, case os:getenv("TCPSRV_DST_HOST") of false -> "localhost"; _ -> os:getenv("TCPSRV_DST_HOST") end).
-define(EL_HOST_PORT, case os:getenv("TCPSRV_EL_HOST_PORT") of false -> ?DST_HOST ++ ":9200"; _ -> os:getenv("TCPSRV_EL_HOST_PORT") end).
-define(EL_INDEX, case os:getenv("TCPSRV_EL_INDEX") of false -> "log"; _ -> os:getenv("TCPSRV_EL_INDEX") end).
-define(EL_MAPPING, case os:getenv("TCPSRV_EL_MAPPING") of false -> "logs"; _ -> os:getenv("TCPSRV_EL_MAPPING") end).
-define(SOLR_HOST_PORT, case os:getenv("TCPSRV_SOLR_HOST_PORT") of false -> ?DST_HOST ++ ":8983"; _ -> os:getenv("TCPSRV_SOLR_HOST_PORT") end).
-define(SOLR_CORE, case os:getenv("TCPSRV_SOLR_CORE") of false -> "logs"; _ -> os:getenv("TCPSRV_SOLR_CORE") end).
-define(REDIS_HOST, ?DST_HOST).
-define(REDIS_PORT, case os:getenv("TCPSRV_REDIS_PORT") of false -> 6379; _ -> os:getenv("TCPSRV_REDIS_PORT") end).

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
    spawn(?MODULE,toDataStore,[?DATASTORE,{Data, inet_parse:ntoa(RemoteAddr)}]).

toDataStore(DataStore, DataTuple) ->
    {Data, IP} = DataTuple,
    {Mega,Sec,Micro} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime({Mega,Sec,Micro}),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",[Year,Month,Day,Hour,Minute,Second,Micro])),
    Body = "{\"message\":\"" ++ subs(binary_to_list(Data), http)
            ++ "\", \"ip\":\"" ++ IP ++ "\", \"timestamp\":\"" ++ StrTime ++ "\"}",
    case DataStore of
        elasticsearch ->
            URL = "http://" ++ ?EL_HOST_PORT ++ "/" ++ ?EL_INDEX ++ "/" ++ ?EL_MAPPING ++ "/",
            spawn(?MODULE,http_post,[URL,Body]);
        redis ->
            Host = ?REDIS_HOST,
            Port = ?REDIS_PORT,
            Key = IP ++ ":" ++ StrTime,
            spawn(?MODULE,tcp_cli, [Host, Port, Key, subs(binary_to_list(Data), socket)]);
        solr ->
            URL = "http://" ++ ?SOLR_HOST_PORT ++ "/solr/" ++ ?SOLR_CORE ++ "/update/json/docs?commit=true",
            spawn(?MODULE,http_post,[URL,Body]);
        _ ->
            donothing
    end.

tcp_cli(Host, Port, Key, Value) ->
    io:format("~p~n", [{Host, Port, Key, Value}]),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "SET \"" ++ Key ++ "\" \"" ++ Value ++ "\"\n"),
    ok = gen_tcp:close(Sock).

http_post(URL, Body) ->
    Method = post,
    Header = [],
    Type = "application/json",
    HTTPOptions = [],
    Options = [],
    R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
    io:format("~p~n", [R]).

subs(Text, socket) ->
    A = re:replace(Text,"\n"," ",[{return,list},global]),
    B = re:replace(A,"\t"," ",[{return,list},global]),
    re:replace(B,"\"","'",[{return,list},global]);
subs(Text, http) ->
    A = re:replace(Text,"\n"," ",[{return,list},global]),
    B = re:replace(A,"\t"," ",[{return,list},global]),
    C = re:replace(B,"\b"," ",[{return,list},global]),
    D = re:replace(C,"\r"," ",[{return,list},global]),
    E = re:replace(D,"\f"," ",[{return,list},global]),
    F = re:replace(E,"\"","'",[{return,list},global]),
    re:replace(F,"\\\\","",[{return,list},global]).