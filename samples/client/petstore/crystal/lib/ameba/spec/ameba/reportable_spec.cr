require "../spec_helper"

module Ameba
  describe Reportable do
    describe "#add_issue" do
      it "adds a new issue for node" do
        s = Source.new "", "source.cr"
        s.add_issue(DummyRule.new, Crystal::Nop.new, "Error!")

        issue = s.issues.first
        issue.rule.should_not be_nil
        issue.location.to_s.should eq ""
        issue.message.should eq "Error!"
      end

      it "adds a new issue by line and column number" do
        s = Source.new "", "source.cr"
        s.add_issue(DummyRule.new, {23, 2}, "Error!")

        issue = s.issues.first
        issue.rule.should_not be_nil
        issue.location.to_s.should eq "source.cr:23:2"
        issue.message.should eq "Error!"
      end
    end

    describe "#valid?" do
      it "returns true if no issues added" do
        s = Source.new "", "source.cr"
        s.should be_valid
      end

      it "returns false if there are issues added" do
        s = Source.new "", "source.cr"
        s.add_issue DummyRule.new, {22, 2}, "ERROR!"
        s.should_not be_valid
      end
    end
  end
end
