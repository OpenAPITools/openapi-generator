require "../../../spec_helper"

module Ameba::Rule::Style
  subject = UnlessElse.new

  describe UnlessElse do
    it "passes if unless hasn't else" do
      s = Source.new %(
        unless something
          :ok
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if unless has else" do
      s = Source.new %(
        unless something
          :one
        else
          :two
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, pos and message" do
      s = Source.new %(
        unless something
          :one
        else
          :two
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid

      issue = s.issues.first
      issue.should_not be_nil
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.to_s.should eq "source.cr:5:3"
      issue.message.should eq "Favour if over unless with else"
    end
  end
end
