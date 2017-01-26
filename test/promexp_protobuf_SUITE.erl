-module(promexp_protobuf_SUITE).

-include("internal_test.hrl").

%% -- callback: ct --
-export([all/0, groups/0]).
-export([encode_bool_test/1, encode_enum_test/1,
         encode_int32_test/1, encode_int64_test/1,
         encode_sint32_test/1, encode_sint64_test/1,
         encode_uint32_test/1, encode_uint64_test/1,
         encode_fixed32_test/1, encode_fixed64_test/1,
         encode_sfixed32_test/1, encode_sfixed64_test/1,
         encode_float_test/1, encode_double_test/1,
         encode_binary_test/1]).

%% == callback: ct ==

all() -> [
          {group, group_public}
         ].

groups() -> [
             {group_public, [parallel], [encode_bool_test, encode_enum_test,
                                         encode_int32_test, encode_int64_test,
                                         encode_sint32_test, encode_sint64_test,
                                         encode_uint32_test, encode_uint64_test,
                                         encode_fixed32_test, encode_fixed64_test,
                                         encode_sfixed32_test, encode_sfixed64_test,
                                         encode_float_test, encode_double_test,
                                         encode_binary_test]}
            ].


encode_bool_test(_Config) ->
    L = [
         %% encode_bool/1
         { [true ], {ok, <<16#01>>} },
         { [false], {ok, <<16#00>>} },
         { [any  ], {error, badarg} },
         %% encode_bool/2
         { [1, true], {ok, <<16#08, 16#01>>} },
         { [0, true], {error, badarg} }
        ],
    [ E = test(encode_bool, A) || {A, E} <- L ].

encode_enum_test(_Config) -> % = encode_int32_test
    L = [
         %% encode_enum/1
         { [0], {ok, <<16#00>>} },
         %% encode_enum/2
         { [1, 0], {ok, <<16#08, 16#00>>} }
        ],
    [ E = test(encode_enum, A) || {A, E} <- L ].

encode_int32_test(_Config) ->
    L = [
         %% encode_int32/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {error, badarg} },
         { [?INT32_MIN -1], {error, badarg} },
         { [?INT32_MIN   ], {ok, <<16#80,16#80,16#80,16#80,16#f8,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [           -1], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [            0], {ok, <<16#00>>} },
         { [            1], {ok, <<16#01>>} },
         { [?INT32_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#07>>} },
         { [?INT32_MAX +1], {error, badarg} },
         { [?UINT32_MAX  ], {error, badarg} },
         { [?UINT32_MAX+1], {error, badarg} },
         { [?INT64_MAX   ], {error, badarg} },
         { [?INT64_MAX +1], {error, badarg} },
         { [?UINT64_MAX  ], {error, badarg} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_int32/2
         { [1, 2], {ok, <<16#08,16#02>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_int32, A) || {A, E} <- L ].

encode_int64_test(_Config) ->
    L = [
         %% encode_int64/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {ok, <<16#80,16#80,16#80,16#80,16#80,16#80,16#80,16#80,16#80,16#01>>} },
         { [?INT32_MIN -1], {ok, <<16#ff,16#ff,16#ff,16#ff,16#f7,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [?INT32_MIN   ], {ok, <<16#80,16#80,16#80,16#80,16#f8,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [           -1], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [            0], {ok, <<16#00>>} },
         { [            1], {ok, <<16#01>>} },
         { [?INT32_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#07>>} },
         { [?INT32_MAX +1], {ok, <<16#80,16#80,16#80,16#80,16#08>>} },
         { [?UINT32_MAX  ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#0f>>} },
         { [?UINT32_MAX+1], {ok, <<16#80,16#80,16#80,16#80,16#10>>} },
         { [?INT64_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#7f>>} },
         { [?INT64_MAX +1], {error, badarg} },
         { [?UINT64_MAX  ], {error, badarg} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_int64/2
         { [1, 2], {ok, <<16#08,16#02>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_int64, A) || {A, E} <- L ].

encode_sint32_test(_Config) ->
    L = [
         %% encode_sint32/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {error, badarg} },
         { [?INT32_MIN -1], {error, badarg} },
         { [?INT32_MIN   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#0f>>} },
         { [           -1], {ok, <<16#01>>} },
         { [            0], {ok, <<16#00>>} },
         { [            1], {ok, <<16#02>>} },
         { [?INT32_MAX   ], {ok, <<16#fe,16#ff,16#ff,16#ff,16#0f>>} },
         { [?INT32_MAX +1], {error, badarg} },
         { [?UINT32_MAX  ], {error, badarg} },
         { [?UINT32_MAX+1], {error, badarg} },
         { [?INT64_MAX   ], {error, badarg} },
         { [?INT64_MAX +1], {error, badarg} },
         { [?UINT64_MAX  ], {error, badarg} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_sint32/2
         { [1, 2], {ok, <<16#08,16#04>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_sint32, A) || {A, E} <- L ].

encode_sint64_test(_Config) ->
    L = [
         %% encode_sint64/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [?INT32_MIN -1], {ok, <<16#81,16#80,16#80,16#80,16#10>>} },
         { [?INT32_MIN   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#0f>>} },
         { [           -1], {ok, <<16#01>>} },
         { [            0], {ok, <<16#00>>} },
         { [            1], {ok, <<16#02>>} },
         { [?INT32_MAX   ], {ok, <<16#fe,16#ff,16#ff,16#ff,16#0f>>} },
         { [?INT32_MAX +1], {ok, <<16#80,16#80,16#80,16#80,16#10>>} },
         { [?UINT32_MAX  ], {ok, <<16#fe,16#ff,16#ff,16#ff,16#1f>>} },
         { [?UINT32_MAX+1], {ok, <<16#80,16#80,16#80,16#80,16#20>>} },
         { [?INT64_MAX   ], {ok, <<16#fe,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [?INT64_MAX +1], {error, badarg} },
         { [?UINT64_MAX  ], {error, badarg} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_sint64/2
         { [1, 2], {ok, <<16#08,16#04>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_sint64, A) || {A, E} <- L ].

encode_uint32_test(_Config) ->
    L = [
         %% encode_uint32/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {error, badarg} },
         { [?INT32_MIN -1], {error, badarg} },
         { [?INT32_MIN   ], {error, badarg} },
         { [           -1], {error, badarg} },
         { [            0], {ok, <<16#00>>} },
         { [            1], {ok, <<16#01>>} },
         { [?INT32_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#07>>} },
         { [?INT32_MAX +1], {ok, <<16#80,16#80,16#80,16#80,16#08>>} },
         { [?UINT32_MAX  ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#0f>>} },
         { [?UINT32_MAX+1], {error, badarg} },
         { [?INT64_MAX   ], {error, badarg} },
         { [?INT64_MAX +1], {error, badarg} },
         { [?UINT64_MAX  ], {error, badarg} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_uint32/2
         { [1, 2], {ok, <<16#08,16#02>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_uint32, A) || {A, E} <- L ].

encode_uint64_test(_Config) ->
    L = [
         %% encode_uint64/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {error, badarg} },
         { [?INT32_MIN -1], {error, badarg} },
         { [?INT32_MIN   ], {error, badarg} },
         { [           -1], {error, badarg} },
         { [            0], {ok, <<16#00>>} },
         { [            1], {ok, <<16#01>>} },
         { [?INT32_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#07>>} },
         { [?INT32_MAX +1], {ok, <<16#80,16#80,16#80,16#80,16#08>>} },
         { [?UINT32_MAX  ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#0f>>} },
         { [?UINT32_MAX+1], {ok, <<16#80,16#80,16#80,16#80,16#10>>} },
         { [?INT64_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#7f>>} },
         { [?INT64_MAX +1], {ok, <<16#80,16#80,16#80,16#80,16#80,16#80,16#80,16#80,16#80,16#01>>} },
         { [?UINT64_MAX  ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#01>>} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_uint64/2
         { [1, 2], {ok, <<16#08,16#02>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_uint64, A) || {A, E} <- L ].

encode_fixed32_test(_Config) ->
    L = [
         %% encode_fixed32/1
         { [?INT32_MIN -1], {error, badarg} },
         { [?INT32_MIN   ], {error, badarg} },
         { [           -1], {error, badarg} },
         { [            0], {ok, <<16#00,16#00,16#00,16#00>>} },
         { [            1], {ok, <<16#01,16#00,16#00,16#00>>} },
         { [?INT32_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#7f>>} },
         { [?INT32_MAX +1], {ok, <<16#00,16#00,16#00,16#80>>} },
         { [?UINT32_MAX  ], {ok, <<16#ff,16#ff,16#ff,16#ff>>} },
         { [?UINT32_MAX+1], {error, badarg} },
         %% encode_fixed32/2
         { [1, 2], {ok, <<16#0d,16#02,16#00,16#00,16#00>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_fixed32, A) || {A, E} <- L ].

encode_fixed64_test(_Config) ->
    L = [
         %% encode_fixed64/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {error, badarg} },
         { [           -1], {error, badarg} },
         { [            0], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [            1], {ok, <<16#01,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [?INT64_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#7f>>} },
         { [?INT64_MAX +1], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#80>>} },
         { [?UINT64_MAX  ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff>>} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_fixed64/2
         { [1, 2], {ok, <<16#09,16#02,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_fixed64, A) || {A, E} <- L ].

encode_sfixed32_test(_Config) ->
    L = [
         %% encode_sfixed32/1
         { [?INT32_MIN -1], {error, badarg} },
         { [?INT32_MIN   ], {ok, <<16#00,16#00,16#00,16#80>>} },
         { [           -1], {ok, <<16#ff,16#ff,16#ff,16#ff>>} },
         { [            0], {ok, <<16#00,16#00,16#00,16#00>>} },
         { [            1], {ok, <<16#01,16#00,16#00,16#00>>} },
         { [?INT32_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#7f>>} },
         { [?INT32_MAX +1], {error, badarg} },
         { [?UINT32_MAX  ], {error, badarg} },
         { [?UINT32_MAX+1], {error, badarg} },
         %% encode_sfixed32/2
         { [1, 2], {ok, <<16#0d,16#02,16#00,16#00,16#00>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_sfixed32, A) || {A, E} <- L ].

encode_sfixed64_test(_Config) ->
    L = [
         %% encode_sfixed64/1
         { [?INT64_MIN -1], {error, badarg} },
         { [?INT64_MIN   ], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#80>>} },
         { [           -1], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff>>} },
         { [            0], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [            1], {ok, <<16#01,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [?INT64_MAX   ], {ok, <<16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#ff,16#7f>>} },
         { [?INT64_MAX +1], {error, badarg} },
         { [?UINT64_MAX  ], {error, badarg} },
         { [?UINT64_MAX+1], {error, badarg} },
         %% encode_sfixed64/2
         { [1, 2], {ok, <<16#09,16#02,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [0, 2], {error, badarg} }
        ],
    [ E = test(encode_sfixed64, A) || {A, E} <- L ].

encode_float_test(_Config) ->
    L = [
         %% encode_float/1
         { [-1.0], {ok, <<16#00,16#00,16#80,16#bf>>} },
         %%[-0.0], {ok, <<16#00,16#00,16#00,16#80>>} },
         { [+0.0], {ok, <<16#00,16#00,16#00,16#00>>} },
         { [+1.0], {ok, <<16#00,16#00,16#80,16#3f>>} },
         %% encode_float/2
         { [1, +2.0], {ok, <<16#0d,16#00,16#00,16#00,16#40>>} },
         { [0, +2.0], {error, badarg} },
         { [1,  2  ], {error, badarg} }
        ],
    [ E = test(encode_float, A) || {A, E} <- L ].

encode_double_test(_Config) ->
    L = [
         %% encode_double/1
         { [-1.0], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#f0,16#bf>>} },
         %%[-0.0], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#80>>} },
         { [+0.0], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00>>} },
         { [+1.0], {ok, <<16#00,16#00,16#00,16#00,16#00,16#00,16#f0,16#3f>>} },
         %% encode_double/2
         { [1, +2.0], {ok, <<16#09,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#40>>} },
         { [0, +2.0], {error, badarg} },
         { [1,  2  ], {error, badarg} }
        ],
    [ E = test(encode_double, A) || {A, E} <- L ].

encode_binary_test(_Config) ->
    L = [
         %% encode_binary/1
         { [<<>>],    {ok, <<16#00>>} },
         { [<<"a">>], {ok, <<16#01,16#61>>} },
         %% encode_binary/2
         { [1, <<"ab">>], {ok, <<16#0a, 16#02,16#61,16#62>>} },
         { [0, <<"ab">>], {error, badarg} },
         { [1, <<1:1>>],  {error, badarg} }
        ],
    [ E = test(encode_binary, A) || {A, E} <- L ].

%% == internal ==

test(Function, Args) ->
    apply(promexp_protobuf, Function, Args).
