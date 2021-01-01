require "../../../spec_helper"

module Ameba::Rule::Lint
  subject = LiteralInInterpolation.new

  describe LiteralInInterpolation do
    it "passes with good interpolation examples" do
      s = Source.new %q(
        name = "Ary"
        "Hello, #{name}"

        "#{name}"

        "Name size: #{name.size}"
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is useless interpolation" do
      [
        %q("#{:Ary}"),
        %q("#{[1, 2, 3]}"),
        %q("#{true}"),
        %q("#{false}"),
        %q("here are #{4} cats"),
      ].each do |str|
        subject.catch(Source.new str).should_not be_valid
      end
    end

    it "reports rule, pos and message" do
      s = Source.new %q(
        "Hello, #{:world} from #{:ameba}"
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 2

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:11"
      issue.end_location.to_s.should eq "source.cr:1:16"
      issue.message.should eq "Literal value found in interpolation"

      issue = s.issues.last
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:26"
      issue.end_location.to_s.should eq "source.cr:1:31"
      issue.message.should eq "Literal value found in interpolation"
    end
  end
end
