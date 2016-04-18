require 'spec_helper'

describe "Store" do
  before do
    @api = Petstore::StoreApi.new(API_CLIENT)
  end

  it "should fetch an order" do
    @order_id = prepare_store(@api)

    item = @api.get_order_by_id(@order_id)
    item.id.should == @order_id

    @api.delete_order(@order_id)
  end

  it "should featch the inventory" do
    result = @api.get_inventory
    result.should be_a(Hash)
    result.should_not be_empty
    result.each do |k, v|
      k.should be_a(Symbol)
      v.should be_a(Integer)
    end
  end

  # mark as pending since original petstore does not return object
  # will re-enable this after updating the petstore server
  xit "should featch the inventory in object" do
    result = @api.get_inventory_in_object
    result.should be_a(Hash)
    result.should_not be_empty
    result.each do |k, v|
      k.should be_a(Symbol)
      v.should be_a(Integer)
    end
  end
end
