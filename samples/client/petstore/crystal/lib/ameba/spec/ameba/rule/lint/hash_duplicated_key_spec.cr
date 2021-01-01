require "../../../spec_helper"

module Ameba::Rule::Lint
  describe HashDuplicatedKey do
    subject = HashDuplicatedKey.new

    it "passes if there is no duplicated keys in a hash literals" do
      s = Source.new %(
        h = {"a" => 1, :a => 2, "b" => 3}
        h = {"a" => 1, "b" => 2, "c" => {"a" => 3, "b" => 4}}
        h = {} of String => String
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is a duplicated key in a hash literal" do
      s = Source.new %q(
        h = {"a" => 1, "b" => 2, "a" => 3}
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is a duplicated key in the inner hash literal" do
      s = Source.new %q(
        h = {"a" => 1, "b" => {"a" => 3, "b" => 4, "a" => 5}}
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, location and message" do
      s = Source.new %q(
        h = {"a" => 1, "a" => 2}
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:5"
      issue.end_location.to_s.should eq "source.cr:1:24"
      issue.message.should eq %(Duplicated keys in hash literal: "a")
    end

    it "reports multiple duplicated keys" do
      s = Source.new %q(
        h = {"key1" => 1, "key1" => 2, "key2" => 3, "key2" => 4}
      )
      subject.catch(s).should_not be_valid
      issue = s.issues.first
      issue.message.should eq %(Duplicated keys in hash literal: "key1", "key2")
    end
  end
end
