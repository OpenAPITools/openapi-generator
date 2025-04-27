defmodule OuterEnumTest do
  use ExUnit.Case, async: true

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
    enum_test =
      JSON.decode!(@valid_json)
      |> then(fn params -> EnumTest.changeset(%EnumTest{}, params) end)
      |> Ecto.Changeset.apply_action!(:insert)

    assert enum_test ==
      %EnumTest{
        enum_string: "UPPER",
        enum_number: 1.1,
        outerEnum: "placed",
        outerEnumInteger: 1
      }
  end
end
