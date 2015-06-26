require 'spec_helper'

describe Petstore::Swagger::Request do

  before(:each) do
    Petstore::Swagger.configure do |config|
      inject_format = true
      config.api_key['api_key'] = 'special-key'
      config.host = 'petstore.swagger.io'
      config.base_path = '/v2'
    end

    @default_http_method = :get
    @default_path = "pet.{format}/fancy"
    @default_params = {
      :params => {:foo => "1", :bar => "2"}
    }
    @request = Petstore::Swagger::Request.new(@default_http_method, @default_path, @default_params)
  end

  describe "initialization" do
    it "sets default response format to json" do
      @request.format.should == 'json'
    end

    it "allows params to be nil" do
      @request = Petstore::Swagger::Request.new(@default_http_method, @default_path, :params => nil)
      @request.params.should == {}
    end

  end

  describe "attr_accessors" do

    it "has working attributes" do
      @request.format.to_s.should == 'json'
    end

    it "allows attributes to be overwritten" do
      @request.http_method.should == :get
      @request.http_method = "post"
      @request.http_method.should == 'post'
    end

  end

  describe "url" do

    it "constructs a full url" do
      @request.url.should == "http://petstore.swagger.io/v2/pet.json/fancy"
    end

  end

  describe "path" do

    it "accounts for a total absence of format in the path string" do
      @request = Petstore::Swagger::Request.new(:get, "/word.{format}/cat/entries", @default_params.merge({
        :format => "xml",
        :params => {
        }
      }))
      @request.url.should == "http://petstore.swagger.io/v2/word.xml/cat/entries"
    end

    it "does string substitution (format) on path params" do
      @request = Petstore::Swagger::Request.new(:get, "/word.{format}/cat/entries", @default_params.merge({
        :format => "xml",
        :params => {
        }
      }))
      @request.url.should == "http://petstore.swagger.io/v2/word.xml/cat/entries"
    end

    it "URI encodes the path" do
      @request = Petstore::Swagger::Request.new(:get, "word.{format}/bill gates/definitions", @default_params.merge({
        :params => {
          :word => "bill gates"
        }
      }))
      @request.url.should =~ /word.json\/bill\%20gates\/definitions/
    end

  end

  describe "#update_params_for_auth!" do
    it "sets header api-key parameter with prefix" do
      Petstore::Swagger.configure do |config|
        inject_format = true
        config.api_key_prefix['api_key'] = 'PREFIX'
      end
      @request.auth_names = ['api_key', 'unknown']
      @request.update_params_for_auth!
      @request.headers['api_key'].should == 'PREFIX special-key'
    end

    it "sets header api-key parameter without prefix" do
      Petstore::Swagger.configure do |config|
        inject_format = true
        config.api_key_prefix['api_key'] = nil
      end
      @request.auth_names = ['api_key', 'unknown']
      @request.update_params_for_auth!
      @request.headers['api_key'].should == 'special-key'
    end
  end

end
