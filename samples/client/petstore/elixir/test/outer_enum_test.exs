defmodule OuterEnumTest do
  use ExUnit.Case, async: true

  alias OpenapiPetstore.Model.EnumTest

  @valid_json """
  {
    "enum_string_required": "lower",
    "enum_string": "UPPER",
    "enum_number": 1.1,
    "outerEnum": "placed",
    "outerEnumInteger": 1
  }
  """

  @tag timeout: :infinity
  test "json_decode/2 with valid JSON" do
    {:ok, enum_test} =
      JSON.decode!(@valid_json)
      |> EnumTest.from_params

    assert enum_test ==
      %EnumTest{
        enum_string_required: "lower",
        enum_string: "UPPER",
        enum_number: 1.1,
        outerEnum: "placed",
        outerEnumInteger: 1
      }
  end
end
