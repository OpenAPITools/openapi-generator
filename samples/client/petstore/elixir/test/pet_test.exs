defmodule PetTest do
    use ExUnit.Case
    alias OpenapiPetstore.Connection
    alias OpenapiPetstore.Api.Pet, as: PetApi
    alias OpenapiPetstore.Model.Pet
    alias OpenapiPetstore.Model.Category
    alias OpenapiPetstore.Model.Tag

    test "add and delete a pet" do
        petId = 10007
        pet = %Pet{
            :id => petId,
            :name => "elixir client test",
            :photoUrls => ["http://test_elixir_unit_test.com"],
            :category => %Category{:id => petId, :name=> "test elixir category"},
            :tags => [%Tag{:id => petId, :name => "test elixir tag"}],
        }
        {:ok, response} = PetApi.add_pet(Connection.new, pet)
        assert response.status == 200

        {:ok, pet} = PetApi.get_pet_by_id(Connection.new, petId)
        assert pet.id == petId
        assert pet.name == "elixir client test"
        assert List.first(pet.photoUrls) == "http://test_elixir_unit_test.com"
        assert pet.category.id == petId
        assert pet.category.name == "test elixir category"
        assert List.first(pet.tags) == %Tag{:id => petId, :name => "test elixir tag"}

        {:ok, response} = PetApi.delete_pet(Connection.new, petId)
        assert response.status == 200
        {:ok, response} = PetApi.get_pet_by_id(Connection.new, petId)
        assert response.status == 404
    end

    test "update a pet" do
        petId = 10007
        pet = %Pet{
            :id => petId,
            :name => "elixir client updatePet",
            :status => "pending",
            :photoUrls => ["http://test_elixir_unit_test.com"]
        }
        {:ok, response} = PetApi.update_pet(Connection.new, pet)
        assert response.status == 200

        {:ok, pet} = PetApi.get_pet_by_id(Connection.new, petId)
        assert pet.id == petId
        assert pet.name == "elixir client updatePet"
        assert pet.status == "pending"
    end

    test "find pet by status" do
        {:ok, listPets} = PetApi.find_pets_by_status(Connection.new, "available")
        assert List.first(listPets) != nil

        {:ok, listPets} = PetApi.find_pets_by_status(Connection.new, "unknown_and_incorrect_status")
        assert List.first(listPets) == nil
    end

end
