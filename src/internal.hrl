-ifndef(internal).
-define(internal, true).

%% == define ==

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

-define(RANGE(V,F,T), (V >= F andalso V =< T)).

-endif. % intenal
