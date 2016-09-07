-module(srv).
-author("Ricardo Cosme <ricjcosme@gmail.com>").
-compile(export_all).
-define(PORT, list_to_integer(os:getenv("TCPSRV_PORT"))).
-define(SERVERS, list_to_integer(os:getenv("TCPSRV_SERVERS"))).
-define(TIMEOUT, list_to_integer(os:getenv("TCPSRV_TIMEOUT"))).
-define(PACKET, list_to_atom(os:getenv("TCPSRV_PACKET"))).
-define(BINARY_OR_LIST, list_to_atom(os:getenv("TCPSRV_BINARY_OR_LIST"))).
%% redis | elasticsearch (default) | solr, others can be added with toDataStore/1
-define(DATASTORE, list_to_atom(os:getenv("TCPSRV_DATASTORE"))). 
-define(DST_HOST, os:getenv("TCPSRV_DST_HOST")).
-define(EL_HOST_PORT, os:getenv("TCPSRV_EL_HOST_PORT")).
-define(EL_INDEX, os:getenv("TCPSRV_EL_INDEX")).
-define(EL_MAPPING, os:getenv("TCPSRV_EL_MAPPING")).
-define(SOLR_HOST_PORT, os:getenv("TCPSRV_SOLR_HOST_PORT")).
-define(SOLR_CORE, os:getenv("TCPSRV_SOLR_CORE")).
-define(REDIS_HOST, ?DST_HOST).
-define(REDIS_PORT, list_to_integer(os:getenv("TCPSRV_REDIS_PORT"))).

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
    re:replace(F,"\\\\","\\\\\\\\",[{return,list},global]).