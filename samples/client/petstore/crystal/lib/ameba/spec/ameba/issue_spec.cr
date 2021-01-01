require "../spec_helper"

module Ameba
  describe Issue do
    it "accepts rule and message" do
      issue = Issue.new rule: DummyRule.new,
        location: nil,
        end_location: nil,
        message: "Blah",
        status: nil

      issue.rule.should_not be_nil
      issue.message.should eq "Blah"
    end

    it "accepts location" do
      location = Crystal::Location.new("path", 3, 2)
      issue = Issue.new rule: DummyRule.new,
        location: location,
        end_location: nil,
        message: "Blah",
        status: nil

      issue.location.to_s.should eq location.to_s
      issue.end_location.should eq nil
    end

    it "accepts end_location" do
      location = Crystal::Location.new("path", 3, 2)
      issue = Issue.new rule: DummyRule.new,
        location: nil,
        end_location: location,
        message: "Blah",
        status: nil

      issue.location.should eq nil
      issue.end_location.to_s.should eq location.to_s
    end

    it "accepts status" do
      issue = Issue.new rule: DummyRule.new,
        location: nil,
        end_location: nil,
        message: "",
        status: :enabled

      issue.status.should eq :enabled
    end
  end
end
