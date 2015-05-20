require 'spec_helper'

describe "Store" do
  before do
    configure_swagger
    prepare_store
  end

  it "should fetch an order" do
    item = SwaggerClient::StoreApi.get_order_by_id(10002)
    item.id.should == 10002
  end
end
