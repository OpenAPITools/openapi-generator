require "../../spec_helper"

module Ameba
  describe Rule::Base do
    describe "#catch" do
      it "accepts and returns source" do
        s = Source.new "", ""
        DummyRule.new.catch(s).should eq s
      end
    end

    describe "#name" do
      it "returns name of the rule" do
        DummyRule.new.name.should eq "Ameba/DummyRule"
      end
    end

    describe "#group" do
      it "returns a group rule belongs to" do
        DummyRule.new.group.should eq "Ameba"
      end
    end
  end

  describe Rule do
    describe ".rules" do
      it "returns a list of all defined rules" do
        Rule.rules.includes?(DummyRule).should be_true
      end
    end
  end
end
