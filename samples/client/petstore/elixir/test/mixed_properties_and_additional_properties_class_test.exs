defmodule MixedPropertiesAndAdditionalPropertiesClass do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Model.MixedPropertiesAndAdditionalPropertiesClass, as: Model
  alias OpenapiPetstore.Model.Animal

  test "decode all properties (not nil)" do
    assert %Model{
             uuid: "3fa85f64-5717-4562-b3fc-2c963f66afa6",
             dateTime: "2013-10-20T19:20:30+01:00",
             map: %{
               "doggie" => %{"className" => "DOG", "color" => "yellow", "breed" => "Shiba Inu"},
               "meow" => %{"className" => "CAT", "color" => "white", "declawed" => false}
             }
           }
           |> Model.decode() ==
             %Model{
               uuid: "3fa85f64-5717-4562-b3fc-2c963f66afa6",
               dateTime: ~U[2013-10-20T18:20:30Z],
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
    assert %Model{
             uuid: nil,
             dateTime: nil,
             map: nil
           }
           |> Model.decode() ==
             %Model{
               uuid: nil,
               dateTime: nil,
               map: nil
             }
  end
end
