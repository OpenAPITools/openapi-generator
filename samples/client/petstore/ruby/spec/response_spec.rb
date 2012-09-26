require 'spec_helper'

describe Swagger::Response do

  before(:each) do

    VCR.use_cassette('pet_resource', :record => :new_episodes) do
      @raw = Typhoeus::Request.get("http://petstore.swagger.wordnik.com/api/pet.json")
    end

    @response = Swagger::Response.new(@raw)
  end

  describe "initialization" do
    it "sets body" do
      @response.body.class.should == Hash
      @response.body.has_key?('apis').should == true
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
        @raw = Typhoeus::Request.get("http://petstore.swagger.wordnik.com/api/pet.xml")
      end
      @response = Swagger::Response.new(@raw)
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

end