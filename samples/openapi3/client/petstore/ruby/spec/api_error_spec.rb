require File.dirname(__FILE__) + '/spec_helper'

describe Petstore::ApiClient do
  describe '#initialize' do
    it "should save the message if one is given" do
      err = Petstore::ApiError.new(message: "Hello")
      expect(err.message).to eq("Hello")
    end

    it "should save the hash as message if no message is given" do
      err = Petstore::ApiError.new(code: 500, response_body: "server error")
      expect(err.message).to eq("{:code=>500, :response_body=>\"server error\"}")
    end
  end
end
