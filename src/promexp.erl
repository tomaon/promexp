-module(promexp).

%% -- public : cowboy --
-export([init/2,           % 2.x
         init/3, handle/2, % 1.x
         terminate/3]).

%% -- public : inets --
-export([metrics/3]).

%% -- internal --
-define(CONTENT_TYPE, "application/vnd.google.protobuf;proto=io.prometheus.client.MetricFamily;encoding=delimited").

%% == public : cowboy ==

-spec init(term(), term()) -> {ok, term(), term()}.
init(Req, State) ->
    Q = cowboy_req:qs(Req),
    R = cowboy_req:reply(200,
                         #{
                           <<"content-type">> => <<?CONTENT_TYPE>>
                          },
                         collect(to_proplists(binary_to_list(Q), "&")),
                         Req),
    {ok, R, State}.

-spec init(term(), term(), term()) -> {ok, term(), term()}.
init(_, Req, _Opts) ->
    {ok, Req, []}.

-spec handle(term(), term()) -> {ok, term(), term()}.
handle(Req, State) ->
    {Q, R1} = cowboy_req:qs(Req),
    {ok, R2} = cowboy_req:reply(200,
                                [
                                 {<<"content-type">>, <<?CONTENT_TYPE>>}
                                ],
                                collect(to_proplists(binary_to_list(Q), "&")),
                                R1),
    {ok, R2, State}.

-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% == public : inets ==

-spec metrics(term(), term(), string()) -> _.
metrics(SessionID, _Env, Input) ->
    B = zlib:gzip(collect(to_proplists(Input, ";"))),
    H = [
         "Content-Encoding: gzip\r\n",
         "Content-Length: ", integer_to_list(size(B)), "\r\n",
         "Content-Type: ", ?CONTENT_TYPE, "\r\n",
         "\r\n"
        ],
    ok = mod_esi:deliver(SessionID, H),
    ok = mod_esi:deliver(SessionID, B).

%% == internal ==

collect(P) ->
    L = [
         {"m", 16#0001, fun promexp_collector_erts:collect_memory/1},
         {"s", 16#03dd, fun promexp_collector_erts:collect_statistics/1},
         {"i", 16#0000, fun promexp_collector_erts:collect_system_info/1}
        ],
    << <<(F(get_value_as_integer(K, P, D)))/binary>> || {K, D, F} <- L >>.

get_value_as_integer(K, L, D) ->
    case lists:keyfind(K, 1, L) of
        {K, V} ->
            list_to_integer(V);
        false ->
            D
    end.

to_proplists(S, L) ->
    lists:map(fun(E) -> list_to_tuple(string:tokens(E, "=")) end, string:tokens(S, L)).
