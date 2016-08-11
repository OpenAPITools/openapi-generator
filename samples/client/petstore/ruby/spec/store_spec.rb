require 'spec_helper'

describe "Store" do
  before do
    @api = Petstore::StoreApi.new(API_CLIENT)
  end

  it "should fetch an order" do
    @order_id = prepare_store(@api)

    item = @api.get_order_by_id(@order_id)
    expect(item.id).to eq(@order_id)

    @api.delete_order(@order_id)
  end

  it "should featch the inventory" do
    result = @api.get_inventory
    expect(result).to be_a(Hash)
    expect(result).not_to be_empty
    result.each do |k, v|
      expect(k).to be_a(Symbol)
      expect(v).to be_a(Integer)
    end
  end

  # mark as pending since original petstore does not return object
  # will re-enable this after updating the petstore server
  xit "should featch the inventory in object" do
    result = @api.get_inventory_in_object
    expect(result).to be_a(Hash)
    expect(result).not_to be_empty
    result.each do |k, v|
      expect(k).to be_a(Symbol)
      expect(v).to be_a(Integer)
    end
  end
end
