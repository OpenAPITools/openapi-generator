# require 'spec_helper'
require File.dirname(__FILE__) + '/spec_helper'

describe Petstore::ApiClient do

  context 'initialization' do

    context 'URL stuff' do

      context 'host' do
        it 'removes http from host' do
          Petstore.configure { |c| c.host = 'http://example.com' }
          Petstore::Configuration.default.host.should == 'example.com'
        end

        it 'removes https from host' do
          Petstore.configure { |c| c.host = 'https://wookiee.com' }
          Petstore::ApiClient.default.config.host.should == 'wookiee.com'
        end

        it 'removes trailing path from host' do
          Petstore.configure { |c| c.host = 'hobo.com/v4' }
          Petstore::Configuration.default.host.should == 'hobo.com'
        end
      end

      context 'base_path' do
        it "prepends a slash to base_path" do
          Petstore.configure { |c| c.base_path = 'v4/dog' }
          Petstore::Configuration.default.base_path.should == '/v4/dog'
        end

        it "doesn't prepend a slash if one is already there" do
          Petstore.configure { |c| c.base_path = '/v4/dog' }
          Petstore::Configuration.default.base_path.should == '/v4/dog'
        end

        it "ends up as a blank string if nil" do
          Petstore.configure { |c| c.base_path = nil }
          Petstore::Configuration.default.base_path.should == ''
        end
      end

    end

  end

  describe "#update_params_for_auth!" do
    it "sets header api-key parameter with prefix" do
      Petstore.configure do |c|
        c.api_key_prefix['api_key'] = 'PREFIX'
        c.api_key['api_key'] = 'special-key'
      end

      api_client = Petstore::ApiClient.new

      config2 = Petstore::Configuration.new do |c|
        c.api_key_prefix['api_key'] = 'PREFIX2'
        c.api_key['api_key'] = 'special-key2'
      end
      api_client2 = Petstore::ApiClient.new(config2)

      auth_names = ['api_key', 'unknown']

      header_params = {}
      query_params = {}
      api_client.update_params_for_auth! header_params, query_params, auth_names
      header_params.should == {'api_key' => 'PREFIX special-key'}
      query_params.should == {}

      header_params = {}
      query_params = {}
      api_client2.update_params_for_auth! header_params, query_params, auth_names
      header_params.should == {'api_key' => 'PREFIX2 special-key2'}
      query_params.should == {}
    end

    it "sets header api-key parameter without prefix" do
      Petstore.configure do |c|
        c.api_key_prefix['api_key'] = nil
        c.api_key['api_key'] = 'special-key'
      end

      api_client = Petstore::ApiClient.new

      header_params = {}
      query_params = {}
      auth_names = ['api_key', 'unknown']
      api_client.update_params_for_auth! header_params, query_params, auth_names
      header_params.should == {'api_key' => 'special-key'}
      query_params.should == {}
    end
  end

  describe "timeout in #build_request" do
    let(:config) { Petstore::Configuration.new }
    let(:api_client) { Petstore::ApiClient.new(config) }

    it "defaults to 0" do
      Petstore::Configuration.default.timeout.should == 0
      config.timeout.should == 0

      request = api_client.build_request(:get, '/test')
      request.options[:timeout].should == 0
    end

    it "can be customized" do
      config.timeout = 100
      request = api_client.build_request(:get, '/test')
      request.options[:timeout].should == 100
    end
  end

  describe "#deserialize" do
    it "handles Array<Integer>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '[12, 34]')
      data = api_client.deserialize(response, 'Array<Integer>')
      data.should be_a(Array)
      data.should == [12, 34]
    end

    it "handles Array<Array<Integer>>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '[[12, 34], [56]]')
      data = api_client.deserialize(response, 'Array<Array<Integer>>')
      data.should be_a(Array)
      data.should == [[12, 34], [56]]
    end

    it "handles Hash<String, String>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '{"message": "Hello"}')
      data = api_client.deserialize(response, 'Hash<String, String>')
      data.should be_a(Hash)
      data.should == {:message => 'Hello'}
    end

    it "handles Hash<String, Pet>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '{"pet": {"id": 1}}')
      data = api_client.deserialize(response, 'Hash<String, Pet>')
      data.should be_a(Hash)
      data.keys.should == [:pet]
      pet = data[:pet]
      pet.should be_a(Petstore::Pet)
      pet.id.should == 1
    end

    it "handles Hash<String, Hash<String, Pet>>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '{"data": {"pet": {"id": 1}}}')
      result = api_client.deserialize(response, 'Hash<String, Hash<String, Pet>>')
      result.should be_a(Hash)
      result.keys.should == [:data]
      data = result[:data]
      data.should be_a(Hash)
      data.keys.should == [:pet]
      pet = data[:pet]
      pet.should be_a(Petstore::Pet)
      pet.id.should == 1
    end
  end

  describe "#object_to_hash" do
    it "ignores nils and includes empty arrays" do
      api_client = Petstore::ApiClient.new
      pet = Petstore::Pet.new
      pet.id = 1
      pet.name = ''
      pet.status = nil
      pet.photo_urls = nil
      pet.tags = []
      expected = {id: 1, name: '', tags: []}
      api_client.object_to_hash(pet).should == expected
    end
  end

  describe "#build_collection_param" do
    let(:param) { ['aa', 'bb', 'cc'] }
    let(:api_client) { Petstore::ApiClient.new }

    it "works for csv" do
      api_client.build_collection_param(param, :csv).should == 'aa,bb,cc'
    end

    it "works for ssv" do
      api_client.build_collection_param(param, :ssv).should == 'aa bb cc'
    end

    it "works for tsv" do
      api_client.build_collection_param(param, :tsv).should == "aa\tbb\tcc"
    end

    it "works for pipes" do
      api_client.build_collection_param(param, :pipes).should == 'aa|bb|cc'
    end

    it "works for multi" do
      api_client.build_collection_param(param, :multi).should == ['aa', 'bb', 'cc']
    end

    it "fails for invalid collection format" do
      proc { api_client.build_collection_param(param, :INVALID) }.should raise_error
    end
  end

  describe "#json_mime?" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      api_client.json_mime?(nil).should == false
      api_client.json_mime?('').should == false

      api_client.json_mime?('application/json').should == true
      api_client.json_mime?('application/json; charset=UTF8').should == true
      api_client.json_mime?('APPLICATION/JSON').should == true

      api_client.json_mime?('application/xml').should == false
      api_client.json_mime?('text/plain').should == false
      api_client.json_mime?('application/jsonp').should == false
    end
  end

  describe "#select_header_accept" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      api_client.select_header_accept(nil).should == nil
      api_client.select_header_accept([]).should == nil

      api_client.select_header_accept(['application/json']).should == 'application/json'
      api_client.select_header_accept(['application/xml', 'application/json; charset=UTF8']).should == 'application/json; charset=UTF8'
      api_client.select_header_accept(['APPLICATION/JSON', 'text/html']).should == 'APPLICATION/JSON'

      api_client.select_header_accept(['application/xml']).should == 'application/xml'
      api_client.select_header_accept(['text/html', 'application/xml']).should == 'text/html,application/xml'
    end
  end

  describe "#select_header_content_type" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      api_client.select_header_content_type(nil).should == 'application/json'
      api_client.select_header_content_type([]).should == 'application/json'

      api_client.select_header_content_type(['application/json']).should == 'application/json'
      api_client.select_header_content_type(['application/xml', 'application/json; charset=UTF8']).should == 'application/json; charset=UTF8'
      api_client.select_header_content_type(['APPLICATION/JSON', 'text/html']).should == 'APPLICATION/JSON'
      api_client.select_header_content_type(['application/xml']).should == 'application/xml'
      api_client.select_header_content_type(['text/plain', 'application/xml']).should == 'text/plain'
    end
  end

  describe "#sanitize_filename" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      api_client.sanitize_filename('sun').should == 'sun'
      api_client.sanitize_filename('sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('../sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('/var/tmp/sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('./sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('..\sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('\var\tmp\sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('c:\var\tmp\sun.gif').should == 'sun.gif'
      api_client.sanitize_filename('.\sun.gif').should == 'sun.gif'
    end
  end

end
