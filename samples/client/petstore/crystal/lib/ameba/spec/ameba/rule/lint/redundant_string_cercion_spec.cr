require "../../../spec_helper"

module Ameba::Rule::Lint
  describe RedundantStringCoercion do
    subject = RedundantStringCoercion.new

    it "does not report if there is no redundant string coersion" do
      s = Source.new %(
        "Hello, #{name}"
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is a redundant string coersion" do
      s = Source.new %q(
        "Hello, #{name.to_s}"
      )
      subject.catch(s).should_not be_valid
    end

    it "does not report if coersion is used in binary op" do
      s = Source.new %q(
        "Hello, #{3.to_s + 's'}"
      )
      subject.catch(s).should be_valid
    end

    it "reports if coercion is used with symbol literals" do
      s = Source.new %q("Hello, #{:symbol.to_s}")
      subject.catch(s).should_not be_valid
    end

    it "reports if coercion is used with number literals" do
      s = Source.new %q("Hello, #{42.to_s}")
      subject.catch(s).should_not be_valid
    end

    it "reports if coercion is used with boolean literals" do
      s = Source.new %q("Hello, #{false.to_s}")
      subject.catch(s).should_not be_valid
    end

    it "reports if coercion is used with char literals" do
      s = Source.new %q("Hello, #{'t'.to_s}")
      subject.catch(s).should_not be_valid
    end

    it "reports redundant coercion in regex" do
      s = Source.new %q(
        /\w #{name.to_s}/
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if Object#to_s is called with arguments" do
      s = Source.new %q(
        /\w #{name.to_s(io)}/
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if Object#to_s is called without receiver" do
      s = Source.new %q(
        /\w #{to_s}/
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if Object#to_s is called with named args" do
      s = Source.new %q(
        "0x#{250.to_s base: 16}"
      )
      subject.catch(s).should be_valid
    end

    it "reports rule, location and message" do
      s = Source.new %q(
        "Hello, #{name1.to_s}"
        "Hello, #{name2.to_s}"
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 2

      issue = s.issues[0]
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:17"
      issue.end_location.to_s.should eq "source.cr:1:20"
      issue.message.should eq RedundantStringCoercion::MSG

      issue = s.issues[1]
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:17"
      issue.end_location.to_s.should eq "source.cr:2:20"
      issue.message.should eq RedundantStringCoercion::MSG
    end
  end
end
