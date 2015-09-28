require 'spec_helper'
require 'json'

describe "Pet" do
  before do
    @pet_api = Petstore::PetApi.new(API_CLIENT)
    prepare_pet @pet_api
  end

  describe "pet methods" do
    it "should construct a new pet object" do
      tag1 = Petstore::Tag.new({'id' => 1, 'name'=> 'tag1'})
      tag2 = Petstore::Tag.new({'id' => 2, 'name'=> 'tag2'})
      category1 = Petstore::Category.new({:id => 1, :name => 'category unknown'})
      # initalize using both string and symbol key
      pet_hash = {
        :id => 10002,
        :name => "RUBY UNIT TESTING",
        :status => "pending",
        :photo_urls => ["url1", "url2"],
        :category => category1,
        :tags => [tag1, tag2]
      }
      pet = Petstore::Pet.new(pet_hash)
      # test new
      pet.name.should == "RUBY UNIT TESTING"
      pet.status.should == "pending"
      pet.id.should == 10002
      pet.tags[0].id.should == 1
      pet.tags[1].name.should == 'tag2'
      pet.category.name.should == 'category unknown'

      # test build_from_hash
      pet2 = Petstore::Pet.new
      pet2.build_from_hash(pet.to_hash)
      pet.to_hash.should == pet2.to_hash

      # make sure sub-object has different object id
      pet.tags[0].object_id.should_not == pet2.tags[0].object_id
      pet.tags[1].object_id.should_not == pet2.tags[1].object_id
      pet.category.object_id.should_not == pet2.category.object_id
    end

    it "should fetch a pet object" do
      pet = @pet_api.get_pet_by_id(10002)
      pet.should be_a(Petstore::Pet)
      pet.id.should == 10002
      pet.name.should == "RUBY UNIT TESTING"
      pet.tags[0].name.should == "tag test"
      pet.category.name.should == "category test"
    end

    it "should not find a pet that does not exist" do
      begin
        @pet_api.get_pet_by_id(-10002)
        fail 'it should raise error'
      rescue Petstore::ApiError => e
        e.code.should == 404
        e.message.should == 'Not Found'
        e.response_body.should == '{"code":1,"type":"error","message":"Pet not found"}'
        e.response_headers.should be_a(Hash)
        e.response_headers['Content-Type'].should == 'application/json'
      end
    end

    it "should find pets by status" do
      pets = @pet_api.find_pets_by_status(:status => 'available')
      pets.length.should >= 3
      pets.each do |pet|
        pet.should be_a(Petstore::Pet)
        pet.status.should == 'available'
      end
    end

    it "should not find a pet with invalid status" do
      pets = @pet_api.find_pets_by_status(:status => 'invalid-status')
      pets.length.should == 0
    end

    it "should find a pet by status" do
      pets = @pet_api.find_pets_by_status(:status => "available,sold")
      pets.each do |pet|
        if pet.status != 'available' && pet.status != 'sold'
          raise "pet status wasn't right"
        end
      end
    end

    it "should update a pet" do
      pet = Petstore::Pet.new({'id' => 10002, 'status' => 'sold'})
      @pet_api.add_pet(:body => pet)

      fetched = @pet_api.get_pet_by_id(10002)
      fetched.id.should == 10002
      fetched.status.should == 'sold'
    end

    it "should create a pet" do
      pet = Petstore::Pet.new('id' => 10002, 'name' => "RUBY UNIT TESTING")
      result = @pet_api.add_pet(:body => pet)
      # nothing is returned
      result.should be_nil

      pet = @pet_api.get_pet_by_id(10002)
      pet.id.should == 10002
      pet.name.should == "RUBY UNIT TESTING"
    end

    it "should upload a file to a pet" do
      pet = Petstore::Pet.new('id' => 10002, 'name' => "RUBY UNIT TESTING")
      result = @pet_api.add_pet(:body => pet)
      # nothing is returned
      result.should be_nil

      result = @pet_api.upload_file(10002, file: File.new('hello.txt'))
      # nothing is returned
      result.should be_nil
    end

    it "should upload a file with form parameter to a pet" do
      pet = Petstore::Pet.new('id' => 10002, 'name' => 'RUBY UNIT TESTING')
      result = @pet_api.add_pet(body: pet)
      result.should be_nil

      result = @pet_api.upload_file(10002, file: File.new('hello.txt'), additional_metadata: 'metadata')
      result.should be_nil
    end
  end
end
