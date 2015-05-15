require 'spec_helper'
require 'json'

describe "Pet" do
  before do
    configure_swagger
    prepare_pet
  end
  
  describe "pet methods" do
    it "should construct a new pet object" do
      tag1 = Tag.new({'id' => 1, 'name'=> 'tag1'})
      tag2 = Tag.new({'id' => 2, 'name'=> 'tag2'})
      category1 = Category.new({:id => 1, :name => 'category unknown'})
      # initalize using both string and symbol key
      pet_hash = {:'id' => 10002, :'name' => "RUBY UNIT TESTING", :'status' => "pending",
              :'photo_urls' => ["url1", "url2"], :'category' => category1,
              :'tags' => [tag1, tag2]}
      pet = Pet.new(pet_hash)
      # test new
      pet.name.should == "RUBY UNIT TESTING"
      pet.status.should == "pending"
      pet.id.should == 10002
      pet.tags[0].id.should == 1
      pet.tags[1].name.should == 'tag2'
      pet.category.name.should == 'category unknown'

      # test build_from_hash
      pet2 = Pet.new
      pet2.build_from_hash(pet.to_hash)
      pet.to_hash.should == pet2.to_hash

      # make sure sub-object has different object id
      pet.tags[0].object_id.should_not == pet2.tags[0].object_id
      pet.tags[1].object_id.should_not == pet2.tags[1].object_id
      pet.category.object_id.should_not == pet2.category.object_id

      puts pet.to_json
      pet_array = [pet, pet2]
      puts pet_array.map{ |v| 
        if v.respond_to?(:to_hash)
          v.to_hash
        else
          v
        end
      }.to_json

    end

    it "should fetch a pet object" do
      pet = PetApi.get_pet_by_id(10002)
      print pet.inspect
      pet.should be_a(Pet)
      pet.id.should == 10002
      pet.name.should == "RUBY UNIT TESTING"
      pet.tags[0].name.should == "RUBY UNIT TESTING"
      pet.category.name.should == "RUBY UNIT TESTING"
    end

    it "should find pets by status" do
      pets = PetApi.find_pets_by_status(:status => 'available')
      pets.length.should >= 3
    end
    
    it "should not find a pet with invalid status" do
      pets = PetApi.find_pets_by_status(:status => 'invalid-status')
      pets.length.should == 0
    end

    it "should find a pet by status" do
      pets = PetApi.find_pets_by_status(:status => "available,sold")
      pets.map {|pet| 
        if(pet.status != 'available' && pet.status != 'sold') 
          raise "pet status wasn't right"
        end
      }
    end
    
    it "should update a pet" do
      pet = Pet.new({'id' => 10002, 'status' => 'sold'})
      PetApi.add_pet(:body => pet)
      
      fetched = PetApi.get_pet_by_id(10002)
      fetched.id.should == 10002
      fetched.status.should == 'sold'
    end

    it "should create a pet" do 
      pet = Pet.new('id' => 10002, 'name' => "RUBY UNIT TESTING")
      PetApi.add_pet(:body => pet)

      pet = PetApi.get_pet_by_id(10002)
      pet.id.should == 10002
      pet.name.should == "RUBY UNIT TESTING"
    end
  end
end
