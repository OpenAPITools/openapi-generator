require "./spec_helper"

describe Petstore do
  describe "#configure" do
    it "works" do
      Petstore.configure do |config|
        config.username = "xxx"
      end

      config = Petstore.configure

      config.should eq Petstore::Configuration.default
      config.username.should eq "xxx"

      # Clean up
      Petstore::Configuration.default.username = nil
    end
  end
end
