require 'spec_helper'

describe "Store" do
  before do
    configure_swagger
    prepare_store
  end

  it "should fetch an order" do
    item = StoreApi.getOrderById(10002)
    item.id.should == 10002
  end
end
