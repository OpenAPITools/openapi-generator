defmodule FormatTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Model.FormatTest

  test "decode all properties (not nil)" do
    assert %FormatTest{
             integer: 1,
             int32: 2,
             int64: 3,
             number: 4.1,
             float: 5.2,
             double: 6.3,
             decimal: "7.4",
             string: "Hello world!",
             byte: "U3dhZ2dlciByb2Nrcw==",
             binary: <<1, 2, 3>>,
             date: "2013-10-20",
             dateTime: "2013-10-20T19:20:30+01:00",
             uuid: "3fa85f64-5717-4562-b3fc-2c963f66afa6",
             password: "green?horse",
             pattern_with_digits: "1234567890",
             pattern_with_digits_and_delimiter: "Image_01"
           }
           |> FormatTest.decode() ==
             %FormatTest{
               integer: 1,
               int32: 2,
               int64: 3,
               number: 4.1,
               float: 5.2,
               double: 6.3,
               decimal: "7.4",
               string: "Hello world!",
               byte: "U3dhZ2dlciByb2Nrcw==",
               binary: <<1, 2, 3>>,
               date: ~D[2013-10-20],
               dateTime: ~U[2013-10-20T18:20:30Z],
               uuid: "3fa85f64-5717-4562-b3fc-2c963f66afa6",
               password: "green?horse",
               pattern_with_digits: "1234567890",
               pattern_with_digits_and_delimiter: "Image_01"
             }
  end

  test "decode all properties (some are nil)" do
    assert %FormatTest{
             integer: nil,
             int32: nil,
             int64: nil,
             number: 4.1,
             float: nil,
             double: nil,
             decimal: nil,
             string: nil,
             byte: "U3dhZ2dlciByb2Nrcw==",
             binary: nil,
             date: "2013-10-20",
             dateTime: nil,
             uuid: nil,
             password: "green?horse",
             pattern_with_digits: nil,
             pattern_with_digits_and_delimiter: nil
           }
           |> FormatTest.decode() ==
             %FormatTest{
               integer: nil,
               int32: nil,
               int64: nil,
               number: 4.1,
               float: nil,
               double: nil,
               decimal: nil,
               string: nil,
               byte: "U3dhZ2dlciByb2Nrcw==",
               binary: nil,
               date: ~D[2013-10-20],
               dateTime: nil,
               uuid: nil,
               password: "green?horse",
               pattern_with_digits: nil,
               pattern_with_digits_and_delimiter: nil
             }
  end
end
