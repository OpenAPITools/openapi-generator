require "../../../spec_helper"

module Ameba::Rule::Lint
  describe UnneededDisableDirective do
    subject = UnneededDisableDirective.new

    it "passes if there are no comments" do
      s = Source.new %(
        a = 1
      )
      subject.catch(s).should be_valid
    end

    it "passes if there is disable directive" do
      s = Source.new %(
        a = 1 # my super var
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if there is disable directive and it is needed" do
      s = Source.new %Q(
        # ameba:disable #{NamedRule.name}
        a = 1
      )
      s.add_issue NamedRule.new, location: {2, 1},
        message: "Useless assignment", status: :disabled
      subject.catch(s).should be_valid
    end

    it "passes if there is inline disable directive and it is needed" do
      s = Source.new %Q(
        a = 1 # ameba:disable #{NamedRule.name}
      )
      s.add_issue NamedRule.new, location: {1, 1},
        message: "Alarm!", status: :disabled
      subject.catch(s).should be_valid
    end

    it "ignores commented out disable directive" do
      s = Source.new %Q(
        # # ameba:disable #{NamedRule.name}
        a = 1
      )
      s.add_issue NamedRule.new, location: {3, 1},
        message: "Alarm!", status: :disabled
      subject.catch(s).should be_valid
    end

    it "fails if there is unneeded directive" do
      s = Source.new %Q(
        # ameba:disable #{NamedRule.name}
        a = 1
      )
      subject.catch(s).should_not be_valid
      s.issues.first.message.should eq(
        "Unnecessary disabling of #{NamedRule.name}"
      )
    end

    it "fails if there is inline unneeded directive" do
      s = Source.new %Q(a = 1 # ameba:disable #{NamedRule.name})
      subject.catch(s).should_not be_valid
      s.issues.first.message.should eq(
        "Unnecessary disabling of #{NamedRule.name}"
      )
    end

    it "detects mixed inline directives" do
      s = Source.new %Q(
        # ameba:disable Rule1, Rule2
        a = 1 # ameba:disable Rule3
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 2
      s.issues.first.message.should contain "Rule1, Rule2"
      s.issues.last.message.should contain "Rule3"
    end

    it "fails if there is disabled UnneededDisableDirective" do
      s = Source.new %Q(
        # ameba:disable #{UnneededDisableDirective.rule_name}
        a = 1
      ), "source.cr"
      s.add_issue UnneededDisableDirective.new, location: {3, 1},
        message: "Alarm!", status: :disabled
      subject.catch(s).should_not be_valid
    end

    it "reports issue, location and message" do
      s = Source.new %Q(
        # ameba:disable Rule1, Rule2
        a = 1
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.message.should eq "Unnecessary disabling of Rule1, Rule2"
    end
  end
end
