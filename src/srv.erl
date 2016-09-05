-module(srv).
-author("Ricardo Cosme <ricjcosme@gmail.com>").
-compile(export_all).
-define(PORT, 10001).
-define(SERVERS, 10).
-define(TIMEOUT, 60000).
-define(PACKET, line).
-define(BINARY_OR_LIST, binary).
-define(DATASTORE, elasticsearch). %% redis | elasticsearch (default) | solr, others can be added with toDataStore/1
-define(DST_HOST, "localhost").
-define(EL_HOST_PORT, ?DST_HOST ++ ":9200").
-define(EL_INDEX, "log").
-define(EL_MAPPING, "logs").
-define(SOLR_HOST_PORT, ?DST_HOST ++ ":8983").
-define(SOLR_CORE, "logs").
-define(REDIS_HOST, ?DST_HOST).
-define(REDIS_PORT, 6379).

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
    Body = "{\"message\":\"" ++ subs(binary_to_list(Data)) 
            ++ "\", \"ip\":\"" ++ IP ++ "\", \"timestamp\":\"" ++ StrTime ++ "\"}",
    case DataStore of
        elasticsearch ->
            URL = "http://" ++ ?EL_HOST_PORT ++ "/" ++ ?EL_INDEX ++ "/" ++ ?EL_MAPPING ++ "/",
            spawn(?MODULE,http_post,[URL,Body]);
        redis ->
            Host = ?REDIS_HOST,
            Port = ?REDIS_PORT,
            Key = IP ++ ":" ++ StrTime,
            spawn(?MODULE,tcp_cli, [Host, Port, Key, subs(binary_to_list(Data))]);
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

subs(Text) ->
    A = re:replace(Text,"\n"," ",[{return,list}]),
    re:replace(A,"\t"," ",[{return,list}]).