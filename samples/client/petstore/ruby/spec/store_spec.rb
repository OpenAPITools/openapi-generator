require 'spec_helper'

describe "Store" do
  before do
    configure_swagger
    prepare_store
  end

  it "should fetch an order" do
    item = Petstore::StoreApi.get_order_by_id(10002)
    item.id.should == 10002
  end

  it "should featch the inventory" do
    result = Petstore::StoreApi.get_inventory
    result.should be_a(Hash)
    result.should_not be_empty
    result.each do |k, v|
      k.should be_a(Symbol)
      v.should be_a(Integer)
    end
  end
end
