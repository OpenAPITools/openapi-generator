require 'spec_helper'

describe "Pet" do
  before do
    configure_swagger
    prepare_pet
  end
  
  describe "pet methods" do
    it "should fetch a pet object" do
      pet = PetApi.getPetById(10002)
      pet.should be_a(Pet)
      pet.id.should == 10002
      pet.name.should == "RUBY UNIT TESTING"
    end

    it "should find pets by status" do
      pets = PetApi.findPetsByStatus('available')
      pets.length.should >= 3
    end
    
    it "should not find a pet with invalid status" do
      pets = PetApi.findPetsByStatus('invalid-status')
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
      pet = Pet.new({'id' => 10002, 'status' => 'sold'})
      PetApi.addPet(pet)
      
      fetched = PetApi.getPetById(10002)
      fetched.id.should == 10002
      fetched.status.should == 'sold'
    end

    it "should create a pet" do 
      pet = Pet.new('id' => 10002, 'name' => "RUBY UNIT TESTING")
      PetApi.addPet(pet)

      pet = PetApi.getPetById(10002)
      pet.id.should == 10002
      pet.name.should == "RUBY UNIT TESTING"
    end
  end
end
