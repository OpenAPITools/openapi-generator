# require 'spec_helper'
require File.dirname(__FILE__) + '/spec_helper'

describe Petstore::ApiClient do

  context 'initialization' do

    context 'URL stuff' do

      context 'host' do
        it 'removes http from host' do
          Petstore.configure { |c| c.host = 'http://example.com' }
          Petstore.configure.host.should == 'example.com'
        end

        it 'removes https from host' do
          Petstore.configure { |c| c.host = 'https://wookiee.com' }
          Petstore.configure.host.should == 'wookiee.com'
        end

        it 'removes trailing path from host' do
          Petstore.configure { |c| c.host = 'hobo.com/v4' }
          Petstore.configure.host.should == 'hobo.com'
        end
      end

      context 'base_path' do
        it "prepends a slash to base_path" do
          Petstore.configure { |c| c.base_path = 'v4/dog' }
          Petstore.configure.base_path.should == '/v4/dog'
        end

        it "doesn't prepend a slash if one is already there" do
          Petstore.configure { |c| c.base_path = '/v4/dog' }
          Petstore.configure.base_path.should == '/v4/dog'
        end

        it "ends up as a blank string if nil" do
          Petstore.configure { |c| c.base_path = nil }
          Petstore.configure.base_path.should == ''
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
      
      header_params = {}
      query_params = {}
      auth_names = ['api_key', 'unknown']
      api_client.update_params_for_auth! header_params, query_params, auth_names
      header_params.should == {'api_key' => 'PREFIX special-key'}
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

  describe "#deserialize" do
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

end
