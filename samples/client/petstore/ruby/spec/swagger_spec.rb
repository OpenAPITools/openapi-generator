# require 'spec_helper'
require File.dirname(__FILE__) + '/spec_helper'

describe Swagger do

  before(:each) do
    configure_swagger
  end
  
  after(:each) do
  end

  context 'initialization' do
    
    context 'URL stuff' do

      context 'host' do
        it 'removes http from host' do
          Swagger.configure {|c| c.host = 'http://example.com' } 
          Swagger.configuration.host.should == 'example.com'
        end

        it 'removes https from host' do
          Swagger.configure {|c| c.host = 'https://wookiee.com' } 
          Swagger.configuration.host.should == 'wookiee.com'
        end

        it 'removes trailing path from host' do
          Swagger.configure {|c| c.host = 'hobo.com/v4' } 
          Swagger.configuration.host.should == 'hobo.com'
        end
      end
      
      context 'base_path' do
        it "prepends a slash to base_path" do
          Swagger.configure {|c| c.base_path = 'v4/dog' } 
          Swagger.configuration.base_path.should == '/v4/dog'
        end
        
        it "doesn't prepend a slash if one is already there" do
          Swagger.configure {|c| c.base_path = '/v4/dog' }
          Swagger.configuration.base_path.should == '/v4/dog'
        end
        
        it "ends up as a blank string if nil" do
          Swagger.configure {|c| c.base_path = nil }
          Swagger.configuration.base_path.should == ''
        end
        
      end

    end
        
  end
    
end