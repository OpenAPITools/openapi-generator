require "../spec_helper"

module Ameba
  describe InlineComments do
    describe InlineComments::COMMENT_DIRECTIVE_REGEX do
      subject = InlineComments::COMMENT_DIRECTIVE_REGEX

      it "allows to parse action and rule name" do
        result = subject.match("# ameba:enable Group/RuleName")
        result = result.should_not be_nil
        result["action"].should eq "enable"
        result["rules"].should eq "Group/RuleName"
      end

      it "parses multiple rules" do
        result = subject.match("# ameba:enable Group/RuleName, OtherRule, Foo/Bar")
        result = result.should_not be_nil
        result["action"].should eq "enable"
        result["rules"].should eq "Group/RuleName, OtherRule, Foo/Bar"
      end

      it "fails to parse directives with spaces" do
        result = subject.match("# ameba  :  enable     Group/RuleName")
        result.should be_nil
      end
    end

    it "disables a rule with a comment directive" do
      s = Source.new %Q(
        # ameba:disable #{NamedRule.name}
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {1, 12}, message: "Error!")
      s.should be_valid
    end

    it "disables a rule with a line that ends with a comment directive" do
      s = Source.new %Q(
        Time.epoch(1483859302) # ameba:disable #{NamedRule.name}
      )
      s.add_issue(NamedRule.new, location: {1, 12}, message: "Error!")
      s.should be_valid
    end

    it "does not disable a rule of a different name" do
      s = Source.new %Q(
        # ameba:disable WrongName
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {2, 12}, message: "Error!")
      s.should_not be_valid
    end

    it "disables a rule if multiple rule names provided" do
      s = Source.new %Q(
        # ameba:disable SomeRule LargeNumbers #{NamedRule.name} SomeOtherRule
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {2, 12}, message: "")
      s.should be_valid
    end

    it "disables a rule if multiple rule names are separated by comma" do
      s = Source.new %Q(
        # ameba:disable SomeRule, LargeNumbers, #{NamedRule.name}, SomeOtherRule
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {2, 12}, message: "")
      s.should be_valid
    end

    it "does not disable if multiple rule names used without required one" do
      s = Source.new %(
        # ameba:disable SomeRule, SomeOtherRule LargeNumbers
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {2, 12}, message: "")
      s.should_not be_valid
    end

    it "does not disable if comment directive has wrong place" do
      s = Source.new %Q(
        # ameba:disable #{NamedRule.name}
        #
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {3, 12}, message: "")
      s.should_not be_valid
    end

    it "does not disable if comment directive added to the wrong line" do
      s = Source.new %Q(
        if use_epoch? # ameba:disable #{NamedRule.name}
          Time.epoch(1483859302)
        end
      )
      s.add_issue(NamedRule.new, location: {3, 12}, message: "")
      s.should_not be_valid
    end

    it "does not disable if that is not a comment directive" do
      s = Source.new %Q(
        "ameba:disable #{NamedRule.name}"
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {3, 12}, message: "")
      s.should_not be_valid
    end

    it "does not disable if that is a commented out directive" do
      s = Source.new %Q(
        # # ameba:disable #{NamedRule.name}
        Time.epoch(1483859302)
      )
      s.add_issue(NamedRule.new, location: {3, 12}, message: "")
      s.should_not be_valid
    end

    it "does not disable if that is an inline commented out directive" do
      s = Source.new %Q(
        a = 1 # Disable it: # ameba:disable #{NamedRule.name}
      )
      s.add_issue(NamedRule.new, location: {2, 12}, message: "")
      s.should_not be_valid
    end

    context "with group name" do
      it "disables one rule with a group" do
        s = Source.new %Q(
          a = 1 # ameba:disable #{DummyRule.rule_name}
        )
        s.add_issue(DummyRule.new, location: {1, 12}, message: "")
        s.should be_valid
      end

      it "doesn't disable others rules" do
        s = Source.new %Q(
          a = 1 # ameba:disable #{DummyRule.rule_name}
        )
        s.add_issue(NamedRule.new, location: {2, 12}, message: "")
        s.should_not be_valid
      end

      it "disables a hole group of rules" do
        s = Source.new %Q(
          a = 1 # ameba:disable #{DummyRule.group_name}
        )
        s.add_issue(DummyRule.new, location: {1, 12}, message: "")
        s.should be_valid
      end

      it "does not disable rules which do not belong to the group" do
        s = Source.new %Q(
          a = 1 # ameba:disable Lint
        )
        s.add_issue(DummyRule.new, location: {2, 12}, message: "")
        s.should_not be_valid
      end
    end
  end
end
