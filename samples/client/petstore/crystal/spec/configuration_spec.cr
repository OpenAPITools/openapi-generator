require "./spec_helper"

describe Petstore::Configuration do
  describe "#initialize" do
    context "with block" do
      it "works" do
        config = Petstore::Configuration.new do |config|
          config.username = "xxx"
        end

        config.username.should eq "xxx"
      end
    end
  end
end
