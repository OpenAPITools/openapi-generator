defmodule DeserializerTest do
  use ExUnit.Case, async: true
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
    "status": "available"
  }
  """

  test "can deserialize valid JSON" do
    pet =
      JSON.decode!(@valid_json)
      |> then(fn params -> Pet.changeset(%Pet{}, params) end)
      |> Ecto.Changeset.apply_action!(:insert)

    assert pet ==
      %Pet{
        id: 14,
        category: %Category{id: 75, name: "sea dragons"},
        name: "Nagga",
        photoUrls: ["https://example.com/nagga1.jpg", "https://example.com/nagga2.jpg"],
        tags: [%Tag{id: 99, name: "dragon"}, %Tag{id: 23, name: "sea"}],
        status: "available"
      }
  end

  test "cannot deserialize invalid JSON" do
    assert JSON.decode(~s/{: 1}/) == {:error, {:invalid_byte, 1, 58}}
  end
end
