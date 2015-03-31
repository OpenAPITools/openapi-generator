require 'spec_helper'

describe "Pet" do
  before do
    Swagger.configure do |config|
      config.api_key = 'special-key' 
      config.host = 'petstore.swagger.io'
      config.base_path = '/v2'
    end
  end
  
  describe "pet methods" do
    it "should fetch a pet object" do
      pet = PetApi.getPetById(5)
      pet.id.should == 5
      pet.name.should == "Dog 2"
    end

    it "should find pets by status" do
      pets = PetApi.findPetsByStatus('available')
      pets.length.should >= 3
    end
    
    it "should not find a pet with invalid status" do
      pets = PetApi.findPetsByStatus('dead')
      pets.length.should == 0
    end

    it "should find a pet by status" do
      pets = PetApi.findPetsByStatus("available,sold")
      pets.map {|pet| 
        if(pet.status != 'available' && pet.status != 'sold') 
          raise "pet status wasn't right"
        end
      }
    end
    
    it "should update a pet" do
      pet = Pet.new({'id' => 99, 'name' => 'programmer', 'status' => 'coding'})
      PetApi.addPet(pet)
      
      fetched = PetApi.getPetById(99)
      fetched.id.should == 99
    end

    it "should create a pet" do 
      pet = Pet.new('id' => 100, 'name' => "Gorilla")
      #raise pet.inspect
      PetApi.addPet(pet)

      pet = PetApi.getPetById(100)
      pet.id.should == 100
      pet.name.should == "Gorilla"
    end
  end
end

describe "Store" do
  before do
    Swagger.configure do |config|
      config.api_key = 'special-key' 
      config.host = 'petstore.swagger.io'
      config.base_path = '/v2'
    end
  end

  it "should fetch an order" do
    item = StoreApi.getOrderById(2)
    item.id.should == 2
  end
end