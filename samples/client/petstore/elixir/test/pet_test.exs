defmodule PetTest do
  use ExUnit.Case, async: true
  alias OpenapiPetstore.Connection
  alias OpenapiPetstore.Api.Pet, as: PetApi
  alias OpenapiPetstore.Model.Pet
  alias OpenapiPetstore.Model.Category
  alias OpenapiPetstore.Model.Tag

  setup do
    %{connection: Connection.new()}
  end

  test "add and delete a pet", %{connection: connection} do
    petId = 10007

    pet = %Pet{
      :id => petId,
      :name => "elixir client test",
      :photoUrls => ["http://test_elixir_unit_test.com"],
      :category => %Category{:id => petId, :name => "test elixir category"},
      :tags => [%Tag{:id => petId, :name => "test elixir tag"}]
    }

    {:ok, %Tesla.Env{} = response} = PetApi.add_pet(connection, pet)
    assert response.status == 200

    {:ok, pet} = PetApi.get_pet_by_id(connection, petId)
    assert pet.id == petId
    assert pet.name == "elixir client test"
    assert pet.photoUrls == ["http://test_elixir_unit_test.com"]
    assert pet.category == %Category{id: petId, name: "test elixir category"}
    assert pet.tags == [%Tag{:id => petId, :name => "test elixir tag"}]

    {:ok, response} = PetApi.delete_pet(connection, petId)
    assert response.status == 200
    {:ok, response} = PetApi.get_pet_by_id(connection, petId)
    assert response.status == 404
  end

  test "update a pet", %{connection: connection} do
    petId = 10007

    pet = %Pet{
      :id => petId,
      :name => "elixir client updatePet",
      :status => "pending",
      :photoUrls => ["http://test_elixir_unit_test.com"]
    }

    {:ok, response} = PetApi.update_pet(connection, pet)
    assert response.status == 200

    {:ok, pet} = PetApi.get_pet_by_id(connection, petId)
    assert pet.id == petId
    assert pet.name == "elixir client updatePet"
    assert pet.status == "pending"
  end

  test "find pet by status", %{connection: connection} do
    {:ok, listPets} = PetApi.find_pets_by_status(connection, "available")
    assert List.first(listPets) != nil

    {:ok, listPets} = PetApi.find_pets_by_status(connection, "unknown_and_incorrect_status")
    assert List.first(listPets) == nil
  end
end
