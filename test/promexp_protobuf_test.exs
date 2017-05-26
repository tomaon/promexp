defmodule PromexpProtobufTest do
  use ExUnit.Case, async: true

  test "encode_bool" do
    [
      # encode_bool/1
      { [:true ], {:ok, <<0x01>>} },
      { [:false], {:ok, <<0x00>>} },
      { [:any  ], {:error, :badarg} },
      # encode_bool/2
      { [1, :true], {:ok, <<0x08, 0x01>>} },
      { [0, :true], {:error, :badarg} }
    ] |> each(:encode_bool)
  end

  test "encode_enum" do # = encode_int32_test
    [
      # encode_enum/1
      { [0], {:ok, <<0x00>>} },
      # encode_enum/2
      { [1, 0], {:ok, <<0x08, 0x00>>} }
    ] |> each(:encode_enum)
  end

  test "encode_int32" do
    [
      # encode_int32/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:error, :badarg} },
      { [int32_min() -1], {:error, :badarg} },
      { [int32_min()   ], {:ok, <<0x80, 0x80, 0x80, 0x80, 0xf8, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [            -1], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [             0], {:ok, <<0x00>>} },
      { [             1], {:ok, <<0x01>>} },
      { [int32_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x07>>} },
      { [int32_max() +1], {:error, :badarg} },
      { [uint32_max()  ], {:error, :badarg} },
      { [uint32_max()+1], {:error, :badarg} },
      { [int64_max()   ], {:error, :badarg} },
      { [int64_max() +1], {:error, :badarg} },
      { [uint64_max()  ], {:error, :badarg} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_int32/2
      { [1, 2], {:ok, <<0x08, 0x02>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_int32)
  end

  test "encode_int64" do
    [
      # encode_int64/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01>>} },
      { [int32_min() -1], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xf7, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [int32_min()   ], {:ok, <<0x80, 0x80, 0x80, 0x80, 0xf8, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [            -1], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [             0], {:ok, <<0x00>>} },
      { [             1], {:ok, <<0x01>>} },
      { [int32_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x07>>} },
      { [int32_max() +1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x08>>} },
      { [uint32_max()  ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x0f>>} },
      { [uint32_max()+1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x10>>} },
      { [int64_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f>>} },
      { [int64_max() +1], {:error, :badarg} },
      { [uint64_max()  ], {:error, :badarg} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_int64/2
      { [1, 2], {:ok, <<0x08, 0x02>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_int64)
  end

  test "encode_sint32" do
    [
      # encode_sint32/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:error, :badarg} },
      { [int32_min() -1], {:error, :badarg} },
      { [int32_min()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x0f>>} },
      { [            -1], {:ok, <<0x01>>} },
      { [             0], {:ok, <<0x00>>} },
      { [             1], {:ok, <<0x02>>} },
      { [int32_max()   ], {:ok, <<0xfe, 0xff, 0xff, 0xff, 0x0f>>} },
      { [int32_max() +1], {:error, :badarg} },
      { [uint32_max()  ], {:error, :badarg} },
      { [uint32_max()+1], {:error, :badarg} },
      { [int64_max()   ], {:error, :badarg} },
      { [int64_max() +1], {:error, :badarg} },
      { [uint64_max()  ], {:error, :badarg} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_sint32/2
      { [1, 2], {:ok, <<0x08, 0x04>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_sint32)
  end

  test "encode_sint64" do
    [
      # encode_sint64/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [int32_min() -1], {:ok, <<0x81, 0x80, 0x80, 0x80, 0x10>>} },
      { [int32_min()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x0f>>} },
      { [            -1], {:ok, <<0x01>>} },
      { [             0], {:ok, <<0x00>>} },
      { [             1], {:ok, <<0x02>>} },
      { [int32_max()   ], {:ok, <<0xfe, 0xff, 0xff, 0xff, 0x0f>>} },
      { [int32_max() +1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x10>>} },
      { [uint32_max()  ], {:ok, <<0xfe, 0xff, 0xff, 0xff, 0x1f>>} },
      { [uint32_max()+1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x20>>} },
      { [int64_max()   ], {:ok, <<0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [int64_max() +1], {:error, :badarg} },
      { [uint64_max()  ], {:error, :badarg} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_sint64/2
      { [1, 2], {:ok, <<0x08, 0x04>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_sint64)
  end

  test "encode_uint32" do
    [
      # encode_uint32/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:error, :badarg} },
      { [int32_min() -1], {:error, :badarg} },
      { [int32_min()   ], {:error, :badarg} },
      { [            -1], {:error, :badarg} },
      { [             0], {:ok, <<0x00>>} },
      { [             1], {:ok, <<0x01>>} },
      { [int32_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x07>>} },
      { [int32_max() +1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x08>>} },
      { [uint32_max()  ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x0f>>} },
      { [uint32_max()+1], {:error, :badarg} },
      { [int64_max()   ], {:error, :badarg} },
      { [int64_max() +1], {:error, :badarg} },
      { [uint64_max()  ], {:error, :badarg} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_uint32/2
      { [1, 2], {:ok, <<0x08, 0x02>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_uint32)
  end

  test "encode_uint64" do
    [
      # encode_uint64/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:error, :badarg} },
      { [int32_min() -1], {:error, :badarg} },
      { [int32_min()   ], {:error, :badarg} },
      { [            -1], {:error, :badarg} },
      { [             0], {:ok, <<0x00>>} },
      { [             1], {:ok, <<0x01>>} },
      { [int32_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x07>>} },
      { [int32_max() +1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x08>>} },
      { [uint32_max()  ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0x0f>>} },
      { [uint32_max()+1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x10>>} },
      { [int64_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f>>} },
      { [int64_max() +1], {:ok, <<0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x01>>} },
      { [uint64_max()  ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01>>} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_uint64/2
      { [1, 2], {:ok, <<0x08, 0x02>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_uint64)
  end

  test "encode_fixed32" do
    [
      # encode_fixed32/1
      { [int32_min() -1], {:error, :badarg} },
      { [int32_min()   ], {:error, :badarg} },
      { [            -1], {:error, :badarg} },
      { [             0], {:ok, <<0x00, 0x00, 0x00, 0x00>>} },
      { [             1], {:ok, <<0x01, 0x00, 0x00, 0x00>>} },
      { [int32_max()   ], {:ok, <<0xff, 0xff, 0xff, 0x7f>>} },
      { [int32_max() +1], {:ok, <<0x00, 0x00, 0x00, 0x80>>} },
      { [uint32_max()  ], {:ok, <<0xff, 0xff, 0xff, 0xff>>} },
      { [uint32_max()+1], {:error, :badarg} },
      # encode_fixed32/2
      { [1, 2], {:ok, <<0x0d, 0x02, 0x00, 0x00, 0x00>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_fixed32)
  end

  test "encode_fixed64" do
    [
      # encode_fixed64/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:error, :badarg} },
      { [            -1], {:error, :badarg} },
      { [             0], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [             1], {:ok, <<0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [int64_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f>>} },
      { [int64_max() +1], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80>>} },
      { [uint64_max()  ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff>>} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_fixed64/2
      { [1, 2], {:ok, <<0x09, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_fixed64)
  end

  test "encode_sfixed32" do
    [
      # encode_sfixed32/1
      { [int32_min() -1], {:error, :badarg} },
      { [int32_min()   ], {:ok, <<0x00, 0x00, 0x00, 0x80>>} },
      { [            -1], {:ok, <<0xff, 0xff, 0xff, 0xff>>} },
      { [             0], {:ok, <<0x00, 0x00, 0x00, 0x00>>} },
      { [             1], {:ok, <<0x01, 0x00, 0x00, 0x00>>} },
      { [int32_max()   ], {:ok, <<0xff, 0xff, 0xff, 0x7f>>} },
      { [int32_max() +1], {:error, :badarg} },
      { [uint32_max()  ], {:error, :badarg} },
      { [uint32_max()+1], {:error, :badarg} },
      # encode_sfixed32/2
      { [1, 2], {:ok, <<0x0d, 0x02, 0x00, 0x00, 0x00>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_sfixed32)
  end

  test "encode_sfixed64" do
    [
      # encode_sfixed64/1
      { [int64_min() -1], {:error, :badarg} },
      { [int64_min()   ], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80>>} },
      { [            -1], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff>>} },
      { [             0], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [             1], {:ok, <<0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [int64_max()   ], {:ok, <<0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f>>} },
      { [int64_max() +1], {:error, :badarg} },
      { [uint64_max()  ], {:error, :badarg} },
      { [uint64_max()+1], {:error, :badarg} },
      # encode_sfixed64/2
      { [1, 2], {:ok, <<0x09, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [0, 2], {:error, :badarg} }
    ] |> each(:encode_sfixed64)
  end

  test "encode_float" do
    [
      # encode_float/1
      { [-1.0], {:ok, <<0x00, 0x00, 0x80, 0xbf>>} },
      # [-0.0], {:ok, <<0x00, 0x00, 0x00, 0x80>>} },
      { [+0.0], {:ok, <<0x00, 0x00, 0x00, 0x00>>} },
      { [+1.0], {:ok, <<0x00, 0x00, 0x80, 0x3f>>} },
      # encode_float/2
      { [1, +2.0], {:ok, <<0x0d, 0x00, 0x00, 0x00, 0x40>>} },
      { [0, +2.0], {:error, :badarg} },
      { [1,  2  ], {:error, :badarg} }
    ] |> each(:encode_float)
  end

  test "encode_double" do
    [
      # encode_double/1
      { [-1.0], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0xbf>>} },
      # [-0.0], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80>>} },
      { [+0.0], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>} },
      { [+1.0], {:ok, <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f>>} },
      # encode_double/2
      { [1, +2.0], {:ok, <<0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40>>} },
      { [0, +2.0], {:error, :badarg} },
      { [1,  2  ], {:error, :badarg} }
    ] |> each(:encode_double)
  end

  test "encode_binary" do
    [
      # encode_binary/1
      { [<<>>],    {:ok, <<0x00>>} },
      { [<<"a">>], {:ok, <<0x01, 0x61>>} },
      # encode_binary/2
      { [1, <<"ab">>], {:ok, <<0x0a, 0x02, 0x61, 0x62>>} },
      { [0, <<"ab">>], {:error, :badarg} },
      # [1, <<1:1>>],  {:error, :badarg} }
    ] |> each(:encode_binary)
  end

  # -- internal --

  defp each(enum, fun) do
    Enum.each(enum, fn({args, term}) -> assert term == apply(:promexp_protobuf, fun, args) end)
  end

  defp int32_max()  do           2147483647 end
  defp int32_min()  do          -2147483648 end
  defp int64_max()  do  9223372036854775807 end
  defp int64_min()  do -9223372036854775808 end
  defp uint32_max() do           4294967295 end
  defp uint64_max() do 18446744073709551615 end

end
