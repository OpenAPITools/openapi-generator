defmodule DeserializerTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Deserializer
  alias OpenapiPetstore.Model.{Category, Pet, Tag}

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
    "status": "foo"
  }
  """

  test "jason_decode/2 with valid JSON" do
    assert Deserializer.jason_decode(@valid_json, Pet) ==
             {:ok,
              %Pet{
                id: 14,
                category: %Category{id: 75, name: "sea dragons"},
                name: "Nagga",
                photoUrls: ["https://example.com/nagga1.jpg", "https://example.com/nagga2.jpg"],
                tags: [%Tag{id: 99, name: "dragon"}, %Tag{id: 23, name: "sea"}],
                status: "foo"
              }}
  end

  test "jason_decode/2 with invalid JSON" do
    assert Deserializer.jason_decode(~s/{: 1}/, Pet) ==
             {:error, %Jason.DecodeError{data: "{: 1}", position: 1, token: nil}}
  end
end
