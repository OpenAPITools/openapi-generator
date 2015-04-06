require 'spec_helper'

describe "Store" do
  before do
    Swagger.configure do |config|
      config.api_key = 'special-key' 
      config.host = 'petstore.swagger.io'
      config.base_path = '/v2'
    end
  end

  it "should fetch an order" do
    item = StoreApi.getOrderById(5)
    item.id.should == 5
  end
end
