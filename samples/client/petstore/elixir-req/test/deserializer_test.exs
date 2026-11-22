defmodule DeserializerTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Model.{Category, Pet, Tag}
  alias OpenapiPetstore.RequestBuilder

  @valid_json """
  {
    "id": 14,
    "category": {
      "id": 75,
      "name": "sea dragons"
    },
    "name": "Nagga",
    "photoUrls": [
      "https://example.com/nagga1.jpg",
      "https://example.com/nagga2.jpg"
    ],
    "tags": [
      {
        "id": 99,
        "name": "dragon"
      },
      {
        "id": 23,
        "name": "sea"
      }
    ],
    "status": "available"
  }
  """

  test "decodes a mapped Req response into an Ecto model" do
    response = %Req.Response{status: 200, body: @valid_json}

    assert RequestBuilder.evaluate_response({:ok, response}, [{200, Pet}]) ==
             {:ok,
              %Pet{
                id: 14,
                category: %Category{id: 75, name: "sea dragons"},
                name: "Nagga",
                photoUrls: ["https://example.com/nagga1.jpg", "https://example.com/nagga2.jpg"],
                tags: [%Tag{id: 99, name: "dragon"}, %Tag{id: 23, name: "sea"}],
                status: "available"
              }}
  end

  test "returns the JSON error for a malformed mapped Req response" do
    response = %Req.Response{status: 200, body: ~s/{: 1}/}

    assert RequestBuilder.evaluate_response({:ok, response}, [{200, Pet}]) ==
             {:error, {:invalid_byte, 1, 58}}
  end

  test "returns the changeset error for an invalid model in a mapped Req response" do
    response = %Req.Response{status: 200, body: "[{}]"}

    assert {:error, %Ecto.Changeset{valid?: false}} =
             RequestBuilder.evaluate_response({:ok, response}, [{200, Pet}])
  end
end
