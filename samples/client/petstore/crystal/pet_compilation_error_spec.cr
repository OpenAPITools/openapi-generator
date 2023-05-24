require "./spec/spec_helper"
require "json"
require "time"

describe Petstore::Pet do
  describe "test an instance of Pet" do
    it "should fail to compile if any required properties is missing" do
      pet = Petstore::Pet.new(id: nil, category: nil, name: nil, photo_urls: Array(String).new, tags: nil, status: nil)
    end
  end
end
