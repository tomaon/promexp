-module(promexp_collector_erts).

-include("internal.hrl").

%% -- public --
-export([collect_memory/1, collect_statistics/1, collect_system_info/1]).

%% == public ==

-spec collect_memory(integer()) -> binary().
collect_memory(Flag)
  when is_integer(Flag) ->
    collect(Flag, [
                   {16#0001, fun encode_memory/0}
                  ]).

-spec collect_statistics(integer()) -> binary().
collect_statistics(Flag)
  when is_integer(Flag) ->
    collect(Flag, [
                   {16#0001, fun encode_statistics_context_switches/0},
                   {16#0002, fun encode_statistics_exact_reductions/0}, % -> reductions
                   {16#0004, fun encode_statistics_garbage_collection/0},
                   {16#0008, fun encode_statistics_io/0},
                   {16#0010, fun encode_statistics_reductions/0},
                   {16#0020, fun encode_statistics_run_queue/0}, % -> total_run_queue_length
                   {16#0040, fun encode_statistics_runtime/0},
                   {16#0080, fun encode_statistics_total_active_tasks/0},
                   {16#0100, fun encode_statistics_total_run_queue_lengths/0},
                   {16#0200, fun encode_statistics_wall_clock/0}
                  ]).

-spec collect_system_info(integer()) -> binary().
collect_system_info(Flag)
  when is_integer(Flag) ->
    collect(Flag, [
                   {16#0001, fun encode_system_info_creation/0},
                   {16#0002, fun encode_system_info_dets/0},
                   {16#0004, fun encode_system_info_ets/0},
                   {16#0008, fun encode_system_info_port/0},
                   {16#0010, fun encode_system_info_process/0}
                  ]).

%% == internal ==

collect(I, L) ->
    collect(I, <<>>, L).

collect(_, B, []) ->
    B;
collect(I, B, [{K, F}|T]) when I band K =/= 0 ->
    collect(I, <<B/binary, (F())/binary>>, T);
collect(I, B, [_|T]) ->
    collect(I, B, T).

encode_double(I) -> element(2, promexp_protobuf:encode_double(I * 1.0)).

encode_double(K, L) -> encode_double(element(2, lists:keyfind(K, 1, L))).


encode_memory() ->
    L = erlang:memory(),
    <<16#aa, 16#02, % = 298
      ?NAME, 11, "beam_memory",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 5, "total",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(total, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 30,
      ?METRIC_LABELPAIR, 17, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 9, "processes",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(processes, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 35,
      ?METRIC_LABELPAIR, 22, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 14, "processes_used",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(processes_used, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 27,
      ?METRIC_LABELPAIR, 14, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 6, "system",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(system, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 25,
      ?METRIC_LABELPAIR, 12, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 4, "atom",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(atom, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 30,
      ?METRIC_LABELPAIR, 17, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 9, "atom_used",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(atom_used, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 27,
      ?METRIC_LABELPAIR, 14, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 6, "binary",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(binary, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 25,
      ?METRIC_LABELPAIR, 12, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 4, "code",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(code, L))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 24,
      ?METRIC_LABELPAIR, 11, ?LABELPAIR_NAME, 4, "type", ?LABELPAIR_VALUE, 3, "ets",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(ets, L))/binary
    >>.


encode_statistics_context_switches() ->
    {ContextSwitches, 0} = statistics(context_switches),
    <<49,
      ?NAME, 32, "beam_statistics_context_switches",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 11,
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(ContextSwitches))/binary
    >>.

encode_statistics_exact_reductions() ->
    {TotalExactReductions, _} = statistics(exact_reductions),
    <<49,
      ?NAME, 32, "beam_statistics_exact_reductions",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 11,
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(TotalExactReductions))/binary
    >>.

encode_statistics_garbage_collection() ->
    {NumberOfGCs, WordsReclaimed, 0} = statistics(garbage_collection),
    <<114,
      ?NAME, 34, "beam_statistics_garbage_collection",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 34,
      ?METRIC_LABELPAIR, 21, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 13, "number_of_gcs",
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(NumberOfGCs))/binary,
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 36,
      ?METRIC_LABELPAIR, 23, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 15, "words_reclaimed",
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(WordsReclaimed))/binary
    >>.

encode_statistics_io() ->
    {{input, Input}, {output, Output}} = statistics(io),
    <<81,
      ?NAME, 18, "beam_statistics_io",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "input",
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(Input))/binary,
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 27,
      ?METRIC_LABELPAIR, 14, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 6, "output",
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(Output))/binary
    >>.

encode_statistics_reductions() ->
    {TotalReductions, _} = statistics(reductions),
    <<43,
      ?NAME, 26, "beam_statistics_reductions",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 11,
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(TotalReductions))/binary
    >>.

encode_statistics_run_queue() ->
    Value = statistics(run_queue),
    <<42,
      ?NAME, 25, "beam_statistics_run_queue",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 11,
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Value))/binary
    >>.

encode_statistics_runtime() ->
    {TotalRunTime, _} = statistics(runtime),
    <<40,
      ?NAME, 23, "beam_statistics_runtime",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 11,
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(TotalRunTime))/binary
    >>.

encode_statistics_total_active_tasks() ->
    ActiveTasks = statistics(total_active_tasks),
    <<51,
      ?NAME, 34, "beam_statistics_total_active_tasks",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 11,
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(ActiveTasks))/binary
    >>.

encode_statistics_total_run_queue_lengths() ->
    TotalRunQueueLenghts = statistics(total_run_queue_lengths),
    <<56,
      ?NAME, 39, "beam_statistics_total_run_queue_lengths",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 11,
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(TotalRunQueueLenghts))/binary
    >>.

encode_statistics_wall_clock() ->
    {TotalWallclockTime, _} = statistics(wall_clock),
    <<43,
      ?NAME, 26, "beam_statistics_wall_clock",
      ?TYPE, ?TYPE_COUNTER,
      ?METRIC, 11,
      ?METRIC_COUNTER, 9, ?COUNTER_VALUE, (encode_double(TotalWallclockTime))/binary
    >>.


encode_system_info_creation() ->
    Value = erlang:system_info(creation),
    <<42,
      ?NAME, 25, "beam_system_info_creation",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 11,
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Value))/binary
    >>.

encode_system_info_dets() ->
    Count = length(dets:all()),
    <<53,
      ?NAME, 21, "beam_system_info_dets",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "count",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Count))/binary
    >>.

encode_system_info_ets() ->
    Count = length(ets:all()),
    Limit = erlang:system_info(ets_limit),
    <<82,
      ?NAME, 20, "beam_system_info_ets",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "count",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Count))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "limit",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Limit))/binary
    >>.

encode_system_info_port() ->
    Count = erlang:system_info(port_count),
    Limit = erlang:system_info(port_limit),
    <<83,
      ?NAME, 21, "beam_system_info_port",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "count",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Count))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "limit",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Limit))/binary
    >>.

encode_system_info_process() ->
    Count = erlang:system_info(process_count),
    Limit = erlang:system_info(process_limit),
    <<86,
      ?NAME, 24, "beam_system_info_process",
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "count",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Count))/binary,
      ?TYPE, ?TYPE_GAUGE,
      ?METRIC, 26,
      ?METRIC_LABELPAIR, 13, ?LABELPAIR_NAME, 4, "kind", ?LABELPAIR_VALUE, 5, "limit",
      ?METRIC_GAUGE, 9, ?GAUGE_VALUE, (encode_double(Limit))/binary
    >>.
