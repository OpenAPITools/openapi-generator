require 'spec_helper'

describe Petstore::Configuration do
  let(:config) { Petstore::Configuration.default }

  before(:each) do
    Petstore.configure do |c|
      c.host = 'petstore.swagger.io'
      c.base_path = 'v2'
    end
  end

  describe '#base_url' do
    it 'should have the default value' do
      expect(config.base_url).to eq('http://petstore.swagger.io/v2')
    end

    it 'should remove trailing slashes' do
      [nil, '', '/', '//'].each do |base_path|
        config.base_path = base_path
        expect(config.base_url).to eq('http://petstore.swagger.io')
      end
    end
  end

  describe 'server settings' do
    it 'should return an array of server settings' do
      expect(config.auth_settings).not_to be_empty
    end

    it 'should get the first url' do
      url = config.server_url(0, server: "dev-petstore", port: "8080")
      expect(url).to eq("http://dev-petstore.swagger.io:8080/v2")
    end

    it 'should get the first url with default values' do
      url = config.server_url(0)
      expect(url).to eq("http://petstore.swagger.io:80/v2")
    end

    it 'should get the second url with default values' do
      url = config.server_url(1)
      expect(url).to eq("https://localhost:8080/v2")
    end

    it 'should get the second url' do
      url = config.server_url(1, version: "v1")
      expect(url).to eq("https://localhost:8080/v1")
    end

    it 'should raise error due to invalid enum value' do
      expect{config.server_url(1, version: "v6")}.to raise_error(ArgumentError)
    end
  end
end
