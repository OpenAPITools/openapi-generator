require "../spec_helper"

module Ameba::Rule
  describe Base do
    context ".rules" do
      it "returns a list of all rules" do
        rules = Rule.rules
        rules.should_not be_nil
        rules.should contain DummyRule
      end
    end

    context "properties" do
      subject = DummyRule.new

      it "is enabled by default" do
        subject.enabled.should be_true
      end

      it "has a description property" do
        subject.description.should_not be_nil
      end

      it "has excluded property" do
        subject.excluded.should be_nil
      end
    end

    describe "#excluded?" do
      it "returns false if a rule does no have a list of excluded source" do
        DummyRule.new.excluded?(Source.new "", "source.cr").should_not be_true
      end

      it "returns false if source is not excluded from this rule" do
        rule = DummyRule.new
        rule.excluded = %w(some_source.cr)
        rule.excluded?(Source.new "", "another_source.cr").should_not be_true
      end

      it "returns true if source is excluded from this rule" do
        rule = DummyRule.new
        rule.excluded = %w(source.cr)
        rule.excluded?(Source.new "", "source.cr").should be_true
      end

      it "returns true if source matches the wildcard" do
        rule = DummyRule.new
        rule.excluded = %w(**/*.cr)
        rule.excluded?(Source.new "", __FILE__).should be_true
      end

      it "returns false if source does not match the wildcard" do
        rule = DummyRule.new
        rule.excluded = %w(*_spec.cr)
        rule.excluded?(Source.new "", "source.cr").should be_false
      end
    end

    describe ".parsed_doc" do
      it "returns the parsed rule doc" do
        DummyRule.parsed_doc.should eq "Dummy Rule which does nothing."
      end
    end

    describe "#==" do
      it "returns true if rule has the same name" do
        DummyRule.new.should eq(DummyRule.new)
      end

      it "returns false if rule has a different name" do
        DummyRule.new.should_not eq(ScopeRule.new)
      end
    end
  end
end
