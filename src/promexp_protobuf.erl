-module(promexp_protobuf).

-include("internal.hrl").

%% -- public --
-export([encode_bool/1, encode_bool/2, encode_enum/1, encode_enum/2,
         encode_int32/1, encode_int32/2, encode_int64/1, encode_int64/2,
         encode_sint32/1, encode_sint32/2, encode_sint64/1, encode_sint64/2,
         encode_uint32/1, encode_uint32/2, encode_uint64/1, encode_uint64/2,
         encode_fixed32/1, encode_fixed32/2, encode_fixed64/1, encode_fixed64/2,
         encode_sfixed32/1, encode_sfixed32/2, encode_sfixed64/1, encode_sfixed64/2,
         encode_float/1, encode_float/2, encode_double/1, encode_double/2,
         encode_binary/1, encode_binary/2]).

%% == public ==

-spec encode_bool(boolean()) -> {ok, binary()}|{error, _}.
encode_bool(true) ->
    {ok, <<16#01>>};
encode_bool(false) ->
    {ok, <<16#00>>};
encode_bool(_) ->
    {error, badarg}.

-spec encode_bool(pos_integer(), boolean()) -> {ok, binary()}|{error, _}.
encode_bool(FieldNumber, Value) ->
    encode(encode_bool, FieldNumber, 0, Value).

-spec encode_enum(integer()) -> {ok, binary()}|{error, _}.
encode_enum(Value) ->
    encode_int32(Value).

-spec encode_enum(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_enum(FieldNumber, Value) ->
    encode_int32(FieldNumber, Value).

-spec encode_int32(integer()) -> {ok, binary()}|{error, _}.
encode_int32(Value)
  when is_integer(Value), ?RANGE(Value, ?INT32_MIN, ?INT32_MAX) ->
    {ok, encode_varint(Value)};
encode_int32(_) ->
    {error, badarg}.

-spec encode_int32(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_int32(FieldNumber, Value) ->
    encode(encode_int32, FieldNumber, 0, Value).

-spec encode_int64(integer()) -> {ok, binary()}|{error, _}.
encode_int64(Value)
  when is_integer(Value), ?RANGE(Value, ?INT64_MIN, ?INT64_MAX) ->
    {ok, encode_varint(Value)};
encode_int64(_) ->
    {error, badarg}.

-spec encode_int64(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_int64(FieldNumber, Value) ->
    encode(encode_int64, FieldNumber, 0, Value).

-spec encode_sint32(integer()) -> {ok, binary()}|{error, _}.
encode_sint32(Value)
  when is_integer(Value), ?RANGE(Value, ?INT32_MIN, ?INT32_MAX) ->
    {ok, encode_varint(zigzag(Value))};
encode_sint32(_) ->
    {error, badarg}.

-spec encode_sint32(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_sint32(FieldNumber, Value) ->
    encode(encode_sint32, FieldNumber, 0, Value).

-spec encode_sint64(integer()) -> {ok, binary()}|{error, _}.
encode_sint64(Value)
  when is_integer(Value), ?RANGE(Value, ?INT64_MIN, ?INT64_MAX) ->
    {ok, encode_varint(zigzag(Value))};
encode_sint64(_) ->
    {error, badarg}.

-spec encode_sint64(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_sint64(FieldNumber, Value) ->
    encode(encode_sint64, FieldNumber, 0, Value).

-spec encode_uint32(integer()) -> {ok, binary()}|{error, _}.
encode_uint32(Value)
  when is_integer(Value), ?RANGE(Value, ?UINT32_MIN, ?UINT32_MAX) ->
    {ok, encode_varint(Value)};
encode_uint32(_) ->
    {error, badarg}.

-spec encode_uint32(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_uint32(FieldNumber, Value) ->
    encode(encode_uint32, FieldNumber, 0, Value).

-spec encode_uint64(integer()) -> {ok, binary()}|{error, _}.
encode_uint64(Value)
  when is_integer(Value), ?RANGE(Value, ?UINT64_MIN, ?UINT64_MAX) ->
    {ok, encode_varint(Value)};
encode_uint64(_) ->
    {error, badarg}.

-spec encode_uint64(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_uint64(FieldNumber, Value) ->
    encode(encode_uint64, FieldNumber, 0, Value).

-spec encode_fixed32(integer()) -> {ok, binary()}|{error, _}.
encode_fixed32(Value)
  when is_integer(Value), ?RANGE(Value, ?UINT32_MIN, ?UINT32_MAX) ->
    <<B1, B2, B3, B4>> = <<Value:32/integer>>,
    {ok, <<B4, B3, B2, B1>>};
encode_fixed32(_) ->
    {error, badarg}.

-spec encode_fixed32(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_fixed32(FieldNumber, Value) ->
    encode(encode_fixed32, FieldNumber, 5, Value).

-spec encode_fixed64(integer()) -> {ok, binary()}|{error, _}.
encode_fixed64(Value)
  when is_integer(Value), ?RANGE(Value, ?UINT64_MIN, ?UINT64_MAX) ->
    <<B1, B2, B3, B4, B5, B6, B7, B8>> = <<Value:64/integer>>,
    {ok, <<B8, B7, B6, B5, B4, B3, B2, B1>>};
encode_fixed64(_) ->
    {error, badarg}.

-spec encode_fixed64(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_fixed64(FieldNumber, Value) ->
    encode(encode_fixed64, FieldNumber, 1, Value).

-spec encode_sfixed32(integer()) -> {ok, binary()}|{error, _}.
encode_sfixed32(Value)
  when is_integer(Value), ?RANGE(Value, ?INT32_MIN, ?INT32_MAX) ->
    <<B1, B2, B3, B4>> = <<Value:32/signed>>,
    {ok, <<B4, B3, B2, B1>>};
encode_sfixed32(_) ->
    {error, badarg}.

-spec encode_sfixed32(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_sfixed32(FieldNumber, Value) ->
    encode(encode_sfixed32, FieldNumber, 5, Value).

-spec encode_sfixed64(integer()) -> {ok, binary()}|{error, _}.
encode_sfixed64(Value)
  when is_integer(Value), ?RANGE(Value, ?INT64_MIN, ?INT64_MAX) ->
    <<B1, B2, B3, B4, B5, B6, B7, B8>> = <<Value:64/signed>>,
    {ok, <<B8, B7, B6, B5, B4, B3, B2, B1>>};
encode_sfixed64(_) ->
    {error, badarg}.

-spec encode_sfixed64(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_sfixed64(FieldNumber, Value) ->
    encode(encode_sfixed64, FieldNumber, 1, Value).

-spec encode_float(float()) -> {ok, binary()}|{error, _}.
encode_float(Value)
  when is_float(Value) ->
    <<B1, B2, B3, B4>> = <<Value:32/float>>,
    {ok, <<B4, B3, B2, B1>>};
encode_float(_) ->
    {error, badarg}.

-spec encode_float(pos_integer(), integer()) -> {ok, binary()}|{error, _}.
encode_float(FieldNumber, Value) ->
    encode(encode_float, FieldNumber, 5, Value).

-spec encode_double(float()) -> {ok, binary()}|{error, _}.
encode_double(Value)
  when is_float(Value) ->
    <<B1, B2, B3, B4, B5, B6, B7, B8>> = <<Value:64/float>>,
    {ok, <<B8, B7, B6, B5, B4, B3, B2, B1>>};
encode_double(_) ->
    {error, badarg}.

-spec encode_double(pos_integer(), float()) -> {ok, binary()}|{error, _}.
encode_double(FieldNumber, Value) ->
    encode(encode_double, FieldNumber, 1, Value).

-spec encode_binary(binary()) -> {ok, binary()}|{error, _}.
encode_binary(Value)
  when is_binary(Value), byte_size(Value) =< ?UINT32_MAX ->
    {ok, <<(encode_varint(size(Value), <<>>))/binary, Value/binary>>};
encode_binary(_) ->
    {error, badarg}.

-spec encode_binary(pos_integer(), binary()) -> {ok, binary()}|{error, _}.
encode_binary(FieldNumber, Value) ->
    encode(encode_binary, FieldNumber, 2, Value).

%% == internal ==

encode(A, N, I, T)
  when is_integer(N), ?RANGE(N, ?FIELD_MIN, ?FIELD_MAX) ->
    case apply(?MODULE, A, [T]) of
        {ok, B} ->
            {ok, <<(encode_varint(N bsl 3 bor I, <<>>))/binary, B/binary>>};
        Other ->
            Other
    end;
encode(_, _,  _, _) ->
    {error, badarg}.

encode_varint(I)
  when I >= 0->
    encode_varint(I, <<>>);
encode_varint(I) ->
    <<S:1, B1:7, B2:7, B3:7, B4:7, B5:7, B6:7, B7:7, B8:7, B9:7>> = <<I:64/signed>>,
    <<1:1, B9:7, 1:1, B8:7, 1:1, B7:7, 1:1, B6:7, 1:1, B5:7,
      1:1, B4:7, 1:1, B3:7, 1:1, B2:7, 1:1, B1:7, 0:7, S:1>>.

encode_varint(I, B)
  when I < 16#80 ->
    <<B/binary, I>>;
encode_varint(I, B) ->
    encode_varint(I bsr 7, <<B/binary, (I band 16#7f bor 16#80)>>).

zigzag(I) ->
    <<S:64/signed>> = <<((I bsl 1) bxor (I bsr 63)):64/unsigned>>,
    S.
