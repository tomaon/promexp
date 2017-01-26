-module(promexp).

%% -- public --
-export([metrics/3]). % inets mod_esi callback

%% == public ==

-spec metrics(term(), term(), string()) -> _.
metrics(SessionID, _Env, Input) ->
    L = [
         {"m", 16#0001, fun promexp_collector_erts:collect_memory/1},
         {"s", 16#03dd, fun promexp_collector_erts:collect_statistics/1},
         {"i", 16#0000, fun promexp_collector_erts:collect_system_info/1}
        ],
    P = to_proplists(Input, ";"),
    B = zlib:gzip(<< <<(F(get_value_as_integer(K, P, D)))/binary>> || {K, D, F} <- L >>),
    H = [
         "Content-Encoding: gzip\r\n",
         "Content-Length: ", integer_to_list(size(B)), "\r\n",
         "Content-Type: application/vnd.google.protobuf;proto=io.prometheus.client.MetricFamily;encoding=delimited\r\n",
         "\r\n"
        ],
    ok = mod_esi:deliver(SessionID, H),
    ok = mod_esi:deliver(SessionID, B).

%% == internal ==

get_value_as_integer(K, L, D) ->
    case lists:keyfind(K, 1, L) of
        {K, V} ->
            list_to_integer(V);
        false ->
            D
    end.

to_proplists(S, L) ->
    lists:map(fun(E) -> list_to_tuple(string:tokens(E, "=")) end, string:tokens(S, L)).
