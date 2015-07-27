# require 'spec_helper'
require File.dirname(__FILE__) + '/spec_helper'

describe Petstore::ApiClient do

  context 'initialization' do

    context 'URL stuff' do

      context 'host' do
        it 'removes http from host' do
          c = Petstore::ApiClient.new
          c.configure {|c| c.host = 'http://example.com' }
          c.host.should == 'example.com'
        end

        it 'removes https from host' do
          c = Petstore::ApiClient.new {|c| c.host = 'https://wookiee.com' }
          c.host.should == 'wookiee.com'
        end

        it 'removes trailing path from host' do
          c = Petstore::ApiClient.new
          c.configure {|c| c.host = 'hobo.com/v4' }
          c.host.should == 'hobo.com'
        end
      end

      context 'base_path' do
        it "prepends a slash to base_path" do
          c = Petstore::ApiClient.new
          c.configure {|c| c.base_path = 'v4/dog' }
          c.base_path.should == '/v4/dog'
        end

        it "doesn't prepend a slash if one is already there" do
          c = Petstore::ApiClient.new
          c.configure {|c| c.base_path = '/v4/dog' }
          c.base_path.should == '/v4/dog'
        end

        it "ends up as a blank string if nil" do
          c = Petstore::ApiClient.new
          c.configure {|c| c.base_path = nil }
          c.base_path.should == ''
        end
      end

    end

  end

  describe "#update_params_for_auth!" do
    it "sets header api-key parameter with prefix" do
      api_client = Petstore::ApiClient.new do |c|
        c.api_key_prefix['api_key'] = 'PREFIX'
        c.api_key['api_key'] = 'special-key'
      end
      header_params = {}
      query_params = {}
      auth_names = ['api_key', 'unknown']
      api_client.update_params_for_auth! header_params, query_params, auth_names
      header_params.should == {'api_key' => 'PREFIX special-key'}
      query_params.should == {}
    end

    it "sets header api-key parameter without prefix" do
      api_client = Petstore::ApiClient.new do |c|
        c.api_key_prefix['api_key'] = nil
        c.api_key['api_key'] = 'special-key'
      end
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

end
