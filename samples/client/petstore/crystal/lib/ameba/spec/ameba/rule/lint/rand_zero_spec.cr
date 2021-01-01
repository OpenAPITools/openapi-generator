require "../../../spec_helper"

module Ameba::Rule::Lint
  describe RandZero do
    subject = RandZero.new

    it "passes if it is not rand(1) or rand(0)" do
      s = Source.new %(
        rand(1.0)
        rand(0.11)
        rand(2)
      )
      subject.catch(s).should be_valid
    end

    it "fails if it is rand(0)" do
      s = Source.new "rand(0)"
      subject.catch(s).should_not be_valid
    end

    it "fails if it is rand(1)" do
      s = Source.new "rand(1)"
      subject.catch(s).should_not be_valid
    end

    it "reports rule, location and a message" do
      s = Source.new "rand(1)", "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first

      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.to_s.should eq "source.cr:1:7"
      issue.message.should eq "rand(1) always returns 0"
    end
  end
end
