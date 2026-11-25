defmodule MixedPropertiesAndAdditionalPropertiesClass do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Model.MixedPropertiesAndAdditionalPropertiesClass, as: Model
  alias OpenapiPetstore.Model.Animal

  test "decode all properties (not nil)" do
    {:ok, model} =
      %{
        uuid: "3fa85f64-5717-4562-b3fc-2c963f66afa6",
        dateTime: "2013-10-20T19:20:30+01:00",
        map: %{
          "doggie" => %{"className" => "DOG", "color" => "yellow", "breed" => "Shiba Inu"},
          "meow" => %{"className" => "CAT", "color" => "white", "declawed" => false}
        }
      }
      |> Model.from_params

    assert model ==
      %Model{
        uuid: "3fa85f64-5717-4562-b3fc-2c963f66afa6",
        dateTime: ~U[2013-10-20 18:20:30Z],
        map: %{
          # TODO values should be Dog and Cat structs instead of an Animal
          "doggie" => %Animal{
            className: "DOG",
            color: "yellow"
          },
          "meow" => %Animal{
            className: "CAT",
            color: "white"
          }
        }
      }
  end

  test "decode all properties (nil)" do
    {:ok, model} =
      %{
        uuid: nil,
        dateTime: nil,
        map: nil
      }
      |> Model.from_params

    assert model ==
      %Model{
        uuid: nil,
        dateTime: nil,
        map: nil
      }
  end

  test "casts maps of arrays and arrays of maps to model values" do
    dog = %{"className" => "DOG", "color" => "yellow"}
    cat = %{"className" => "CAT", "color" => "white"}
    dog_model = %Animal{className: "DOG", color: "yellow"}
    cat_model = %Animal{className: "CAT", color: "white"}

    assert {:ok, model} =
             Model.from_params(%{
               "map_of_arrays" => %{"shelter" => [dog, cat]},
               "array_of_maps" => [%{"dog" => dog}, %{"cat" => cat}]
             })

    assert model.map_of_arrays == %{"shelter" => [dog_model, cat_model]}
    assert model.array_of_maps == [%{"dog" => dog_model}, %{"cat" => cat_model}]
    assert Ecto.embedded_load(Model, Ecto.embedded_dump(model, :json), :json) == model
  end

  test "reports the map key and array index of an invalid model" do
    assert {:error, %Ecto.Changeset{errors: [map_of_arrays: {_message, details}]}} =
             Model.from_params(%{
               "map_of_arrays" => %{"shelter" => [%{"className" => "DOG"}, %{}]}
             })

    assert details[:source] == ["shelter", 1]
    assert details[:changeset].errors[:className]
  end

  test "accepts model structs and rejects non-object map values" do
    dog = %Animal{className: "DOG"}
    assert {:ok, %Model{map: %{"dog" => ^dog}}} = Model.from_params(%{"map" => %{"dog" => dog}})

    assert {:error, %Ecto.Changeset{errors: [map: {"must be an object", details}]}} =
             Model.from_params(%{"map" => %{"dog" => "DOG"}})

    assert details[:source] == ["dog"]
  end

  test "preserves arbitrary values in free-form maps" do
    free_form = %{"animal" => %{"className" => "DOG"}, "mixed" => [1, true, nil]}

    assert {:ok, %Model{free_form_map: ^free_form, map: %{}}} =
             Model.from_params(%{"free_form_map" => free_form, "map" => %{}})
  end
end
