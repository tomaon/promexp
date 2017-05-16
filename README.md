# [Prometheus](https://prometheus.io/) exporter for Erlang/Elixir

[![Build Status](https://travis-ci.org/tomaon/promexp.svg?branch=master)](https://travis-ci.org/tomaon/promexp)

## Build

```bash
make all
```

## Example

### erlang: inets

```bash
% make run
Eshell V8.3  (abort with ^G)
1> inets:services().
[{httpc,<0.67.0>},{httpd,<0.70.0>}]
2>
```

### prometheus:

```bash
% cd examples
% prometheus
```

## Metrics

### Memory

+ `beam_memory`  
Type: Gauge  
Flag: 16#0001 (default: on)  
Label: type=total|processes|processes_used|system|atom|atom_used|binary|code|ets  
see [erlang:memory/0](http://erlang.org/doc/man/erlang.html#memory-0)  

### Statistics

+ `beam_statistics_context_switches`  
Type: Counter  
Flag: 16#0001 (default: on)  
see [statistics(context_switches)](http://erlang.org/doc/man/erlang.html#statistics_context_switches)  

+ `beam_statistics_exact_reductions`  
Type: Counter  
Flag: 16#0002 (default: off)  
see [statistics(exact_reductions)](http://erlang.org/doc/man/erlang.html#statistics_exact_reductions)  

+ `beam_statistics_garbage_collection`  
Type: Counter  
Flag: 16#0004 (default: on)  
Label: kind=number_of_gcs|words_reclaimed  
see [statistics(garbage_collection)](http://erlang.org/doc/man/erlang.html#statistics_garbage_collection)  

+ `beam_statistics_io`  
Type: Counter  
Flag: 16#0008 (default: on)  
Label: kind=input|output  
see [statistics(io)](http://erlang.org/doc/man/erlang.html#statistics_io)  

+ `beam_statistics_reductions`  
Type: Counter  
Flag: 16#0010 (default: on)  
see [statistics(reductions)](http://erlang.org/doc/man/erlang.html#statistics_reductions)  

+ `beam_statistics_run_queue`  
Type: Gauge  
Flag: 16#0020 (default: off)  
see [statistics(run_queue)](http://erlang.org/doc/man/erlang.html#statistics_run_queue)  

+ `beam_statistics_runtime`  
Type: Counter  
Flag: 16#0040 (default: on)  
see [statistics(runtime)](http://erlang.org/doc/man/erlang.html#statistics_runtime)  

+ `beam_statistics_total_active_tasks`  
Type: Gauge  
Flag: 16#0080 (default: on)  
see [statistics(total_active_tasks)](http://erlang.org/doc/man/erlang.html#statistics_total_active_tasks)  

+ `beam_statistics_total_run_queue_lengths`  
Type: Gauge  
Flag: 16#0100 (default: on)  
see [statistics(total_run_queue_lengths)](http://erlang.org/doc/man/erlang.html#statistics_total_run_queue_lengths)  

+ `beam_statistics_wall_clock`  
Type: Counter  
Flag: 16#0200 (default: on)  
see [statistics(wall_clock)](http://erlang.org/doc/man/erlang.html#statistics_wall_clock)  

### System Info

+ `beam_system_info_creation`  
Type: Gauge  
Flag: 16#0001 (default: off)  
see [erlang:system_info(creation)](http://erlang.org/doc/man/erlang.html#system_info_creation)  

+ `beam_system_info_dets`  
Type: Gauge  
Flag: 16#0002 (default: off)  
Label: kind=count  
see [dets:all/0](http://erlang.org/doc/man/dets.html#all-0)  

+ `beam_system_info_ets`  
Type: Gauge  
Flag: 16#0004 (default: off)  
Label: kind=count|limit  
see [ets:all/0](http://erlang.org/doc/man/ets.html#all-0), [erlang:system_info(ets_limit)](http://erlang.org/doc/man/erlang.html#system_info_ets_limit)  

+ `beam_system_info_port`  
Type: Gauge  
Flag: 16#0008 (default: off)  
Label: kind=count|limit  
see [erlang:system_info(port_count)](http://erlang.org/doc/man/erlang.html#system_info_port_count),
[erlang:system_info(port_limit)](http://erlang.org/doc/man/erlang.html#system_info_port_limit)  

+ `beam_system_info_process`  
Type: Gauge  
Flag: 16#0010 (default: off)  
Label: kind=count|limit  
see [erlang:system_info(process_count)](http://erlang.org/doc/man/erlang.html#system_info_process_count), [erlang:system_info(process_limit)](http://erlang.org/doc/man/erlang.html#system_info_process_limit)  

## License

Apache-2.0
