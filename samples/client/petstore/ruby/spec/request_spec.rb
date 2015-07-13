require 'spec_helper'

describe SwaggerClient::Swagger::Request do

  before(:each) do
    SwaggerClient::Swagger.configure do |config|
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
    @request = SwaggerClient::Swagger::Request.new(@default_http_method, @default_path, @default_params)
  end

  describe "initialization" do

    it "sets default response format to json" do
      @request.format.should == 'json'
    end

    it "sets default headers correctly" do
      @request.headers.should == {'Content-Type' => 'application/json', 'User-Agent' => 'ruby-swagger-1.0.0'}
    end

    it "allows params to be nil" do
      @request = SwaggerClient::Swagger::Request.new(@default_http_method, @default_path, :params => nil)
      @request.query_string.should == ""
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

    it "constructs a query string" do
      @request.query_string.should == "?bar=2&foo=1"
    end

    it "constructs a full url" do
      @request.url.should == "http://petstore.swagger.io/v2/pet.json/fancy?bar=2&foo=1"
    end

  end

  describe "body" do

    it "camelCases parameters" do
      @request = SwaggerClient::Swagger::Request.new(@default_http_method, @default_path, @default_params.merge({
        :body => {
          :bad_dog => 'bud',
          :goodDog => "dud"
        }
      }))
      @request.body.keys.should == [:badDog, :goodDog]
    end

  end

  describe "path" do

    it "accounts for a total absence of format in the path string" do
      @request = SwaggerClient::Swagger::Request.new(:get, "/word.{format}/cat/entries", @default_params.merge({
        :format => "xml",
        :params => {
        }
      }))
      @request.url.should == "http://petstore.swagger.io/v2/word.xml/cat/entries"
    end

    it "does string substitution (format) on path params" do
      @request = SwaggerClient::Swagger::Request.new(:get, "/word.{format}/cat/entries", @default_params.merge({
        :format => "xml",
        :params => {
        }
      }))
      @request.url.should == "http://petstore.swagger.io/v2/word.xml/cat/entries"
    end

    it "leaves path-bound params out of the query string" do
      @request = SwaggerClient::Swagger::Request.new(:get, "/word.{format}/{word}/entries", @default_params.merge({
        :params => {
          :word => "cat",
          :limit => 20
        }
      }))
      @request.query_string.should == "?limit=20"
    end

    it "returns a question-mark free (blank) query string if no query params are present" do
      @request = SwaggerClient::Swagger::Request.new(:get, "/word.{format}/{word}/entries", @default_params.merge({
        :params => {
          :word => "cat",
        }
      }))
      @request.query_string.should == ""
    end

    it "removes blank params" do
      @request = SwaggerClient::Swagger::Request.new(:get, "words/fancy", @default_params.merge({
        :params => {
          :word => "dog",
          :limit => "",
          :foo => "criminy"
        }
      }))
      @request.query_string.should == "?foo=criminy&word=dog"
    end

    it "URI encodes the path" do
      @request = SwaggerClient::Swagger::Request.new(:get, "word.{format}/bill gates/definitions", @default_params.merge({
        :params => {
          :word => "bill gates"
        }
      }))
      @request.url.should =~ /word.json\/bill\%20gates\/definitions/
    end

    it "converts numeric params to strings" do
      @request = SwaggerClient::Swagger::Request.new(@default_http_method, @default_path, @default_params.merge({
        :params => {
          :limit => 100
        }
      }))

      @request.interpreted_path.should_not be_nil
      @request.query_string.should =~ /\?limit=100/
      @request.url.should =~ /\?limit=100/
    end

    it "camelCases parameters" do
      @request = SwaggerClient::Swagger::Request.new(@default_http_method, @default_path, @default_params.merge({
        :params => {
          :bad_dog => 'bud',
          :goodDog => "dud"
        }
      }))
      @request.query_string.should == "?badDog=bud&goodDog=dud"
    end

    it "converts boolean values to their string representation" do
      params = {:stringy => "fish", :truthy => true, :falsey => false}
      @request = SwaggerClient::Swagger::Request.new(:get, 'fakeMethod', :params => params)
      @request.query_string.should == "?falsey=false&stringy=fish&truthy=true"
    end

  end

  describe "#update_params_for_auth!" do
    it "sets header api-key parameter with prefix" do
      SwaggerClient::Swagger.configure do |config|
        inject_format = true
        config.api_key_prefix['api_key'] = 'PREFIX'
      end
      @request.auth_names = ['api_key', 'unknown']
      @request.update_params_for_auth!
      @request.headers['api_key'].should == 'PREFIX special-key'
    end

    it "sets header api-key parameter without prefix" do
      SwaggerClient::Swagger.configure do |config|
        inject_format = true
        config.api_key_prefix['api_key'] = nil
      end
      @request.auth_names = ['api_key', 'unknown']
      @request.update_params_for_auth!
      @request.headers['api_key'].should == 'special-key'
    end
  end

end
