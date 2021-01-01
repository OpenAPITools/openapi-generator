require "../../../spec_helper"

module Ameba::Rule::Lint
  describe BadDirective do
    subject = BadDirective.new

    it "does not report if rule is correct" do
      s = Source.new %(
        # ameba:disable Lint/BadDirective
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is incorrect action" do
      s = Source.new %(
        # ameba:foo Lint/BadDirective
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1

      issue = s.issues.first
      issue.message.should eq(
        "Bad action in comment directive: 'foo'. Possible values: disable, enable"
      )
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.to_s.should eq ""
    end

    it "reports if there are incorrect rule names" do
      s = Source.new %(
        # ameba:enable BadRule1, BadRule2
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1

      issue = s.issues.first
      issue.message.should eq(
        "Such rules do not exist: BadRule1, BadRule2"
      )
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.to_s.should eq ""
    end

    it "does not report if there no action and rules at all" do
      s = Source.new %(
        # ameba:
      )
      subject.catch(s).should be_valid
    end

    it "does not report if there are no rules" do
      s = Source.new %(
        # ameba:enable
        # ameba:disable
      )
      subject.catch(s).should be_valid
    end

    it "does not report if there are group names in the directive" do
      s = Source.new %(
        # ameba:disable Style Performance
      )
      subject.catch(s).should be_valid
    end
  end
end
