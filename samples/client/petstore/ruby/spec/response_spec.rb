require 'spec_helper'

describe Petstore::Swagger::Response do

  before do
    configure_swagger
    prepare_pet
  end

  before(:each) do
    VCR.use_cassette('pet_resource', :record => :new_episodes) do
      @raw = Typhoeus::Request.get("http://petstore.swagger.io/v2/pet/10002")
    end

    @response = Petstore::Swagger::Response.new(@raw)
  end

  describe "initialization" do
    it "sets body" do
      @response.body.should be_a(String)
      data = JSON.parse(@response.body)
      data.should be_a(Hash)
      data['id'].should == 10002
    end

    it "sets code" do
      @response.code.should == 200
    end

    it "converts header string into a hash" do
      @response.headers.class.should == Hash
    end
  end

  describe "format" do
    it "recognizes json" do
      @response.format.should == 'json'
      @response.json?.should == true
    end

    it "recognizes xml" do
      VCR.use_cassette('xml_response_request', :record => :new_episodes) do
        @raw = Typhoeus::Request.get("http://petstore.swagger.io/v2/pet/10002",
                                    :headers => {'Accept'=> "application/xml"})
      end
      @response = Petstore::Swagger::Response.new(@raw)
      @response.format.should == 'xml'
      @response.xml?.should == true
    end
  end

  describe "prettiness" do
    it "has a pretty json body" do
      @response.pretty_body.should =~ /\{.*\}/
    end

    it "has pretty headers" do
      @response.pretty_headers.should =~ /\{.*\}/
    end
  end

  describe "deserialize" do
    it "handles Hash<String, String>" do
      @response.stub(:body) { '{"message": "Hello"}' }
      data = @response.deserialize('Hash<String, String>')
      data.should be_a(Hash)
      data.should == {:message => 'Hello'}
    end

    it "handles Hash<String, Pet>" do
      json = @response.body
      @response.stub(:body) { "{\"pet\": #{json}}" }
      data = @response.deserialize('Hash<String, Pet>')
      data.should be_a(Hash)
      data.keys.should == [:pet]
      pet = data[:pet]
      pet.should be_a(Petstore::Pet)
      pet.id.should == 10002
    end
  end

end
