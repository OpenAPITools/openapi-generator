require 'petstore_helper'
require 'spec_helper'
require 'json'

describe "Pet" do
  before do
    @pet_api = Petstore::PetApi.new(API_CLIENT)
    @pet_id = prepare_pet(@pet_api)
  end

  after do
    # remove the testing pet
    begin
      @pet_api.delete_pet(@pet_id)
    rescue Petstore::ApiError => e
      # ignore ApiError 404 (Not Found)
      raise e if e.code != 404
    end
  end

  describe "pet methods" do
    it "should construct a new pet object" do
      tag1 = Petstore::Tag.new('id' => 1, 'name' => 'tag1')
      tag2 = Petstore::Tag.new('id' => 2, 'name' => 'tag2')
      category1 = Petstore::Category.new(:id => 1, :name => 'category unknown')
      # initalize using both string and symbol key
      pet_hash = {
        :id => @pet_id,
        :name => "RUBY UNIT TESTING",
        :status => "pending",
        :photo_urls => ["url1", "url2"],
        :category => category1,
        :tags => [tag1, tag2]
      }
      pet = Petstore::Pet.new(pet_hash)
      # test new
      expect(pet.name).to eq("RUBY UNIT TESTING")
      expect(pet.status).to eq("pending")
      expect(pet.id).to eq(@pet_id)
      expect(pet.tags[0].id).to eq(1)
      expect(pet.tags[1].name).to eq('tag2')
      expect(pet.category.name).to eq('category unknown')

      # test build_from_hash
      pet2 = Petstore::Pet.new
      pet2.build_from_hash(pet.to_hash)
      expect(pet.to_hash).to eq(pet2.to_hash)

      # make sure sub-object has different object id
      expect(pet.tags[0].object_id).not_to eq(pet2.tags[0].object_id)
      expect(pet.tags[1].object_id).not_to eq(pet2.tags[1].object_id)
      expect(pet.category.object_id).not_to eq(pet2.category.object_id)
    end

    it "should fetch a pet object" do
      pet = @pet_api.get_pet_by_id(@pet_id)
      expect(pet).to be_a(Petstore::Pet)
      expect(pet.id).to eq(@pet_id)
      expect(pet.name).to eq("RUBY UNIT TESTING")
      expect(pet.tags[0].name).to eq("tag test")
      expect(pet.category.name).to eq("category test")
    end

    it "should fetch a pet object with http info" do
      pet, status_code, headers = @pet_api.get_pet_by_id_with_http_info(@pet_id)
      expect(status_code).to eq(200)
      expect(headers['Content-Type']).to eq('application/json')
      expect(pet).to be_a(Petstore::Pet)
      expect(pet.id).to eq(@pet_id)
      expect(pet.name).to eq("RUBY UNIT TESTING")
      expect(pet.tags[0].name).to eq("tag test")
      expect(pet.category.name).to eq("category test")
    end

    it "should not find a pet that does not exist" do
      begin
        @pet_api.get_pet_by_id(-@pet_id)
        fail 'it should raise error'
      rescue Petstore::ApiError => e
        expect(e.code).to eq(404)
        # skip the check as the response contains a timestamp that changes on every reponse
        # expect(e.message).to eq("Error message: the server returns an error\nHTTP status code: 404\nResponse headers: {\"Date\"=>\"Tue, 26 Feb 2019 04:35:40 GMT\", \"Access-Control-Allow-Origin\"=>\"*\", \"Access-Control-Allow-Methods\"=>\"GET, POST, DELETE, PUT\", \"Access-Control-Allow-Headers\"=>\"Content-Type, api_key, Authorization\", \"Content-Type\"=>\"application/json\", \"Connection\"=>\"close\", \"Server\"=>\"Jetty(9.2.9.v20150224)\"}\nResponse body: {\"code\":1,\"type\":\"error\",\"message\":\"Pet not found\"}")
        expect(e.response_body).to eq('{"code":1,"type":"error","message":"Pet not found"}')
        expect(e.response_headers).to include('Content-Type')
        expect(e.response_headers['Content-Type']).to eq('application/json')
      end
    end

    # skip the following as original petstore spec does not have endpoints for testing byte array
    # we will re-enable this after updating the petstore server
    xit "should create and get pet with byte array (binary, string)" do
      pet = @pet_api.get_pet_by_id(@pet_id)
      pet.id = @pet_id + 1
      str = serialize_json(pet)
      @pet_api.add_pet_using_byte_array(body: str)

      fetched_str = @pet_api.pet_pet_idtesting_byte_arraytrue_get(pet.id)
      expect(fetched_str).to be_a(String)
      fetched = deserialize_json(fetched_str, 'Pet')
      expect(fetched).to be_a(Petstore::Pet)
      expect(fetched.id).to eq(pet.id)
      expect(fetched.category).to be_a(Petstore::Category)
      expect(fetched.category.name).to eq(pet.category.name)

      @pet_api.delete_pet(pet.id)
    end

    # skip the following as original petstore spec does not have endpoints for testing byte array
    # we will re-enable this after updating the petstore server
    xit "should get pet in object" do
      pet = @pet_api.get_pet_by_id_in_object(@pet_id)
      expect(pet).to be_a(Petstore::InlineResponse200)
      expect(pet.id).to eq(@pet_id)
      expect(pet.name).to eq("RUBY UNIT TESTING")
      expect(pet.category).to be_a(Hash)
      expect(pet.category[:id]).to eq(20002)
      expect(pet.category[:name]).to eq('category test')
    end

    it "should update a pet" do
      pet = @pet_api.get_pet_by_id(@pet_id)
      expect(pet.id).to eq(@pet_id)
      expect(pet.name).to eq("RUBY UNIT TESTING")
      expect(pet.status).to eq('pending')

      @pet_api.update_pet_with_form(@pet_id, name: 'new name', status: 'sold')

      fetched = @pet_api.get_pet_by_id(@pet_id)
      expect(fetched.id).to eq(@pet_id)
      expect(fetched.name).to eq("new name")
      expect(fetched.status).to eq('sold')
    end

    it "should find pets by status" do
      pets = @pet_api.find_pets_by_status(['available'])
      expect(pets.length).to be >= 3
      pets.each do |pet|
        expect(pet).to be_a(Petstore::Pet)
        expect(pet.status).to eq('available')
      end
    end

    it "should not find a pet with invalid status" do
      pets = @pet_api.find_pets_by_status(['invalid-status'])
      expect(pets.length).to eq(0)
    end

    it "should find a pet by status" do
      pets = @pet_api.find_pets_by_status(["available", "sold"])
      pets.each do |pet|
        if pet.status != 'available' && pet.status != 'sold'
          raise "pet status wasn't right"
        end
      end
    end

    it "should create a pet" do
      id = @pet_id + 1

      pet = Petstore::Pet.new('id' => id, 'name' => "RUBY UNIT TESTING")
      result = @pet_api.add_pet(pet)
      # nothing is returned
      expect(result).to be_nil

      pet = @pet_api.get_pet_by_id(id)
      expect(pet.id).to eq(id)
      expect(pet.name).to eq("RUBY UNIT TESTING")

      @pet_api.delete_pet(id)
    end

    it "should upload a file to a pet" do
      result = @pet_api.upload_file(@pet_id, file: File.new('hello.txt'))
      # ApiResponse is returned
      expect(result).to be_a(Petstore::ApiResponse)
    end

    it "should upload a file with form parameter to a pet" do
      result = @pet_api.upload_file(@pet_id, file: File.new('hello.txt'), additional_metadata: 'metadata')
      # ApiResponse is returned
      expect(result).to be_a(Petstore::ApiResponse)
    end

    it "should implement eql? and hash" do
      pet1 = Petstore::Pet.new
      pet2 = Petstore::Pet.new
      expect(pet1).to eq(pet2)
      expect(pet2).to eq(pet1)
      expect(pet1.eql?(pet2)).to eq(true)
      expect(pet2.eql?(pet1)).to eq(true)
      expect(pet1.hash).to eq(pet2.hash)
      expect(pet1).to eq(pet1)
      expect(pet1.eql?(pet1)).to eq(true)
      expect(pet1.hash).to eq(pet1.hash)

      pet1.name = 'really-happy'
      pet1.photo_urls = ['http://foo.bar.com/1', 'http://foo.bar.com/2']
      expect(pet1).not_to eq(pet2)
      expect(pet2).not_to eq(pet1)
      expect(pet1.eql?(pet2)).to eq(false)
      expect(pet2.eql?(pet1)).to eq(false)
      expect(pet1.hash).not_to eq(pet2.hash)
      expect(pet1).to eq(pet1)
      expect(pet1.eql?(pet1)).to eq(true)
      expect(pet1.hash).to eq(pet1.hash)

      pet2.name = 'really-happy'
      pet2.photo_urls = ['http://foo.bar.com/1', 'http://foo.bar.com/2']
      expect(pet1).to eq(pet2)
      expect(pet2).to eq(pet1)
      expect(pet1.eql?(pet2)).to eq(true)
      expect(pet2.eql?(pet1)).to eq(true)
      expect(pet1.hash).to eq(pet2.hash)
      expect(pet2).to eq(pet2)
      expect(pet2.eql?(pet2)).to eq(true)
      expect(pet2.hash).to eq(pet2.hash)
    end
  end
end
