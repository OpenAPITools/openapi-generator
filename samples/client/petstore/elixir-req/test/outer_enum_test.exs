defmodule OuterEnumTest do
  use ExUnit.Case, async: true

  alias OpenapiPetstore.{Api.Fake, Connection, Model.EnumTest}

  @quoted_enum ~S(value "quoted" #{value})

  defmodule EnumResponseAdapter do
    def run(request) do
      body = Req.Request.get_private(request, :enum_response)
      {request, %Req.Response{status: 200, body: body}}
    end
  end

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

  test "casts named enums in fields, arrays, and maps as wire values" do
    assert {:ok, %EnumTest{
             enum_number: 2.0,
             named_enum: "42",
             named_enum_array: ["true", @quoted_enum],
             named_enum_map: %{"state" => "_abc"}
           }} =
             EnumTest.from_params(%{
               enum_string_required: "lower",
               enum_number: 2,
               named_enum: "42",
               named_enum_array: ["true", @quoted_enum],
               named_enum_map: %{"state" => "_abc"}
             })
  end

  test "validates named enum values in scalar fields and collections" do
    invalid_values = [
      named_enum: "invalid",
      named_enum_array: ["42", "invalid"],
      named_enum_map: %{"state" => "invalid"}
    ]

    for {field, value} <- invalid_values do
      params = %{enum_string_required: "lower"} |> Map.put(field, value)

      assert {:error, %Ecto.Changeset{errors: [{^field, {"is invalid", details}}]}} =
               EnumTest.from_params(params)

      assert details[:validation] == :inclusion
    end
  end

  test "decodes scalar and array enum responses" do
    assert Fake.get_named_enum(connection_with_body("42"), ["true"], "true") == {:ok, "42"}

    assert Fake.get_named_enum_array(connection_with_body(["true", @quoted_enum])) ==
             {:ok, ["true", @quoted_enum]}
  end

  test "returns errors for response enum values outside the schema" do
    assert Fake.get_named_enum(connection_with_body(42), ["true"], "true") ==
             {:error, {:invalid_enum, 42}}

    assert Fake.get_named_enum_array(connection_with_body(["42", "invalid"])) ==
             {:error, {:invalid_enum, ["42", "invalid"]}}
  end

  defp connection_with_body(value) do
    [req_options: [adapter: EnumResponseAdapter]]
    |> Connection.new()
    |> Req.Request.put_private(:enum_response, JSON.encode!(value))
  end
end
