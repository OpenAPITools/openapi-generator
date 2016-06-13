# require 'spec_helper'
require File.dirname(__FILE__) + '/spec_helper'

describe Petstore::ApiClient do

  context 'initialization' do

    context 'URL stuff' do

      context 'host' do
        it 'removes http from host' do
          Petstore.configure { |c| c.host = 'http://example.com' }
          expect(Petstore::Configuration.default.host).to eq('example.com')
        end

        it 'removes https from host' do
          Petstore.configure { |c| c.host = 'https://wookiee.com' }
          expect(Petstore::ApiClient.default.config.host).to eq('wookiee.com')
        end

        it 'removes trailing path from host' do
          Petstore.configure { |c| c.host = 'hobo.com/v4' }
          expect(Petstore::Configuration.default.host).to eq('hobo.com')
        end
      end

      context 'base_path' do
        it "prepends a slash to base_path" do
          Petstore.configure { |c| c.base_path = 'v4/dog' }
          expect(Petstore::Configuration.default.base_path).to eq('/v4/dog')
        end

        it "doesn't prepend a slash if one is already there" do
          Petstore.configure { |c| c.base_path = '/v4/dog' }
          expect(Petstore::Configuration.default.base_path).to eq('/v4/dog')
        end

        it "ends up as a blank string if nil" do
          Petstore.configure { |c| c.base_path = nil }
          expect(Petstore::Configuration.default.base_path).to eq('')
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
      expect(header_params).to eq({'api_key' => 'PREFIX special-key'})
      expect(query_params).to eq({})

      header_params = {}
      query_params = {}
      api_client2.update_params_for_auth! header_params, query_params, auth_names
      expect(header_params).to eq({'api_key' => 'PREFIX2 special-key2'})
      expect(query_params).to eq({})
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
      expect(header_params).to eq({'api_key' => 'special-key'})
      expect(query_params).to eq({})
    end
  end

  describe "timeout in #build_request" do
    let(:config) { Petstore::Configuration.new }
    let(:api_client) { Petstore::ApiClient.new(config) }

    it "defaults to 0" do
      expect(Petstore::Configuration.default.timeout).to eq(0)
      expect(config.timeout).to eq(0)

      request = api_client.build_request(:get, '/test')
      expect(request.options[:timeout]).to eq(0)
    end

    it "can be customized" do
      config.timeout = 100
      request = api_client.build_request(:get, '/test')
      expect(request.options[:timeout]).to eq(100)
    end
  end

  describe "#deserialize" do
    it "handles Array<Integer>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '[12, 34]')
      data = api_client.deserialize(response, 'Array<Integer>')
      expect(data).to be_a(Array)
      expect(data).to eq([12, 34])
    end

    it "handles Array<Array<Integer>>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '[[12, 34], [56]]')
      data = api_client.deserialize(response, 'Array<Array<Integer>>')
      expect(data).to be_a(Array)
      expect(data).to eq([[12, 34], [56]])
    end

    it "handles Hash<String, String>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '{"message": "Hello"}')
      data = api_client.deserialize(response, 'Hash<String, String>')
      expect(data).to be_a(Hash)
      expect(data).to eq({:message => 'Hello'})
    end

    it "handles Hash<String, Pet>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '{"pet": {"id": 1}}')
      data = api_client.deserialize(response, 'Hash<String, Pet>')
      expect(data).to be_a(Hash)
      expect(data.keys).to eq([:pet])
      pet = data[:pet]
      expect(pet).to be_a(Petstore::Pet)
      expect(pet.id).to eq(1)
    end

    it "handles Hash<String, Hash<String, Pet>>" do
      api_client = Petstore::ApiClient.new
      headers = {'Content-Type' => 'application/json'}
      response = double('response', headers: headers, body: '{"data": {"pet": {"id": 1}}}')
      result = api_client.deserialize(response, 'Hash<String, Hash<String, Pet>>')
      expect(result).to be_a(Hash)
      expect(result.keys).to eq([:data])
      data = result[:data]
      expect(data).to be_a(Hash)
      expect(data.keys).to eq([:pet])
      pet = data[:pet]
      expect(pet).to be_a(Petstore::Pet)
      expect(pet.id).to eq(1)
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
      expect(api_client.object_to_hash(pet)).to eq(expected)
    end
  end

  describe "#build_collection_param" do
    let(:param) { ['aa', 'bb', 'cc'] }
    let(:api_client) { Petstore::ApiClient.new }

    it "works for csv" do
      expect(api_client.build_collection_param(param, :csv)).to eq('aa,bb,cc')
    end

    it "works for ssv" do
      expect(api_client.build_collection_param(param, :ssv)).to eq('aa bb cc')
    end

    it "works for tsv" do
      expect(api_client.build_collection_param(param, :tsv)).to eq("aa\tbb\tcc")
    end

    it "works for pipes" do
      expect(api_client.build_collection_param(param, :pipes)).to eq('aa|bb|cc')
    end

    it "works for multi" do
      expect(api_client.build_collection_param(param, :multi)).to eq(['aa', 'bb', 'cc'])
    end

    it "fails for invalid collection format" do
      expect { api_client.build_collection_param(param, :INVALID) }.to raise_error(RuntimeError, 'unknown collection format: :INVALID')
    end
  end

  describe "#json_mime?" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      expect(api_client.json_mime?(nil)).to eq(false)
      expect(api_client.json_mime?('')).to eq(false)

      expect(api_client.json_mime?('application/json')).to eq(true)
      expect(api_client.json_mime?('application/json; charset=UTF8')).to eq(true)
      expect(api_client.json_mime?('APPLICATION/JSON')).to eq(true)

      expect(api_client.json_mime?('application/xml')).to eq(false)
      expect(api_client.json_mime?('text/plain')).to eq(false)
      expect(api_client.json_mime?('application/jsonp')).to eq(false)
    end
  end

  describe "#select_header_accept" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      expect(api_client.select_header_accept(nil)).to eq(nil)
      expect(api_client.select_header_accept([])).to eq(nil)

      expect(api_client.select_header_accept(['application/json'])).to eq('application/json')
      expect(api_client.select_header_accept(['application/xml', 'application/json; charset=UTF8'])).to eq('application/json; charset=UTF8')
      expect(api_client.select_header_accept(['APPLICATION/JSON', 'text/html'])).to eq('APPLICATION/JSON')

      expect(api_client.select_header_accept(['application/xml'])).to eq('application/xml')
      expect(api_client.select_header_accept(['text/html', 'application/xml'])).to eq('text/html,application/xml')
    end
  end

  describe "#select_header_content_type" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      expect(api_client.select_header_content_type(nil)).to eq('application/json')
      expect(api_client.select_header_content_type([])).to eq('application/json')

      expect(api_client.select_header_content_type(['application/json'])).to eq('application/json')
      expect(api_client.select_header_content_type(['application/xml', 'application/json; charset=UTF8'])).to eq('application/json; charset=UTF8')
      expect(api_client.select_header_content_type(['APPLICATION/JSON', 'text/html'])).to eq('APPLICATION/JSON')
      expect(api_client.select_header_content_type(['application/xml'])).to eq('application/xml')
      expect(api_client.select_header_content_type(['text/plain', 'application/xml'])).to eq('text/plain')
    end
  end

  describe "#sanitize_filename" do
    let(:api_client) { Petstore::ApiClient.new }

    it "works" do
      expect(api_client.sanitize_filename('sun')).to eq('sun')
      expect(api_client.sanitize_filename('sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('../sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('/var/tmp/sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('./sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('..\sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('\var\tmp\sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('c:\var\tmp\sun.gif')).to eq('sun.gif')
      expect(api_client.sanitize_filename('.\sun.gif')).to eq('sun.gif')
    end
  end

end
