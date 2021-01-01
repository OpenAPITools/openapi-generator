require "../../../spec_helper"

module Ameba::Rule::Style
  describe IsANil do
    subject = IsANil.new

    it "doesn't report if there are no is_a?(Nil) calls" do
      s = Source.new %(
        a = 1
        a.nil?
        a.is_a?(NilLiteral)
        a.is_a?(Custom::Nil)
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is a call to is_a?(Nil) without receiver" do
      s = Source.new %(
        is_a?(Nil)
      )
      subject.catch(s).should_not be_valid
    end

    it "reports if there is a call to is_a?(Nil) with receiver" do
      s = Source.new %(
        a.is_a?(Nil)
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, location and message" do
      s = Source.new %(
        nil.is_a? Nil
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:11"
      issue.end_location.to_s.should eq "source.cr:1:13"
      issue.message.should eq IsANil::MSG
    end
  end
end
