-ifndef(internal).
-define(internal, true).

%% == define ==

%% Format version 0.0.4
%%  https://github.com/prometheus/client_model/blob/master/metrics.proto

-define(NAME,             16#0a).

-define(TYPE,             16#18).
-define(TYPE_COUNTER,     16#00).
-define(TYPE_GAUGE,       16#01).
-define(TYPE_UNTYPED,     16#03).

-define(METRIC,           16#22).
-define(METRIC_LABELPAIR, 16#0a).
-define(METRIC_GAUGE,     16#12).
-define(METRIC_COUNTER,   16#1a).
-define(METRIC_UNTYPED,   16#2a).

-define(LABELPAIR_NAME,   16#0a).
-define(LABELPAIR_VALUE,  16#12).

-define(COUNTER_VALUE,    16#09).

-define(GAUGE_VALUE,      16#09).

-define(UNTYPED_VALUE,    16#09).


-define(INT32_MAX,            2147483647).
-define(INT32_MIN,           -2147483648).
-define(INT64_MAX,   9223372036854775807).
-define(INT64_MIN,  -9223372036854775808).

-define(UINT32_MAX,           4294967295).
-define(UINT32_MIN,                    0).
-define(UINT64_MAX, 18446744073709551615).
-define(UINT64_MIN,                    0).

-define(FIELD_MIN,                     1).
-define(FIELD_MAX,            4294967295).

-endif. % intenal
