require "./spec_helper"

Spectator.describe Petstore do
  describe "#configure" do
    it "works" do
      Petstore.configure do |config|
        config.username = "xxx"
      end

      config = Petstore.configure

      expect(config).to eq Petstore::Configuration.default
      expect(config.username).to eq "xxx"

      # Clean up
      Petstore::Configuration.default.username = nil
    end
  end
end
