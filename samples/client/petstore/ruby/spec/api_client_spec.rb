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

end
