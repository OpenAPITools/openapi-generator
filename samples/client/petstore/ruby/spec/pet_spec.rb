require 'spec_helper'

describe "Pet" do
  before do
	  Swagger.configure do |config|
	    config.api_key = 'special-key' 
	    config.host = 'petstore.swagger.wordnik.com'
	    config.base_path = '/api'
	  end
  end
  
  describe "pet methods" do
    it "should fetch a pet object" do
      pet = Pet_api.get_pet_by_id(1)
      pet.id.should == 1
      pet.name.should == "Cat 1"
    end

    it "should find pets by status" do
      pets = Pet_api.find_pets_by_status('available')
      pets.length.should >= 3
    end
    
    it "should not find a pet with invalid status" do
      pets = Pet_api.find_pets_by_status('dead')
      pets.length.should == 0
    end

	  it "should find a pet by status" do
	    pets = Pet_api.find_pets_by_status("available,sold")
	    pets.map {|pet| 
	      if(pet.status != 'available' && pet.status != 'sold') 
	        raise "pet status wasn't right"
	      end
	    }
	  end
	  
	  it "should update a pet" do
	    pet = Pet.new({:id => 99, :name => 'programmer', :status => 'coding'})
	    Pet_api.add_pet(pet)
	    
	    fetched = Pet_api.get_pet_by_id(99)
	    fetched.id.should == 99
	  end

    it "should create a pet" do 
      pet = Pet.new({:id => 100, :name => "Gorilla"})
      raise pet.inspect
      Pet_api.add_pet(pet)

      pet = Pet_api.get_pet_by_id(100)
      pet.id.should == 100
    end
  end
end

describe "Store" do
  before do
	  Swagger.configure do |config|
	    config.api_key = 'special-key' 
	    config.host = 'petstore.swagger.wordnik.com'
	    config.base_path = '/api'
	  end
  end

  it "should fetch an order" do
    item = Store_api.get_order_by_id(1)
    item.id.should == 1
  end
end