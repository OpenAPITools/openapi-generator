defmodule OuterEnumTest do
  use ExUnit.Case, async: true

  alias OpenapiPetstore.Deserializer
  alias OpenapiPetstore.Model.EnumTest

  @valid_json """
  {
    "enum_string": "UPPER",
    "enum_number": 1.1,
    "outerEnum": "placed",
    "outerEnumInteger": 1
  }
  """

  @tag timeout: :infinity
  test "json_decode/2 with valid JSON" do
    assert Deserializer.json_decode(@valid_json, EnumTest) ==
             {:ok,
              %EnumTest{
                enum_string: "UPPER",
                enum_number: 1.1,
                outerEnum: "placed",
                outerEnumInteger: 1
              }}
  end
end
