require "../../../spec_helper"

module Ameba::Rule::Lint
  subject = ComparisonToBoolean.new

  describe ComparisonToBoolean do
    it "passes if there is no comparison to boolean" do
      source = Source.new %(
        a = true

        if a
          :ok
        end

        if true
          :ok
        end

        unless s.empty?
          :ok
        end

        :ok if a

        :ok if a != 1

        :ok if a == "true"

        case a
        when true
          :ok
        when false
          :not_ok
        end
      )
      subject.catch(source).should be_valid
    end

    context "boolean on the right" do
      it "fails if there is == comparison to boolean" do
        source = Source.new %(
          if s.empty? == true
            :ok
          end
        )
        subject.catch(source).should_not be_valid
      end

      it "fails if there is != comparison to boolean" do
        source = Source.new %(
          if a != false
            :ok
          end
        )
        subject.catch(source).should_not be_valid
      end

      it "fails if there is case comparison to boolean" do
        source = Source.new %(
          a === true
        )
        subject.catch(source).should_not be_valid
      end

      it "reports rule, pos and message" do
        source = Source.new "a != true", "source.cr"
        subject.catch(source)

        issue = source.issues.first
        issue.rule.should_not be_nil
        issue.location.to_s.should eq "source.cr:1:1"
        issue.message.should eq "Comparison to a boolean is pointless"
      end
    end

    context "boolean on the left" do
      it "fails if there is == comparison to boolean" do
        source = Source.new %(
          if true == s.empty?
            :ok
          end
        )
        subject.catch(source).should_not be_valid
      end

      it "fails if there is != comparison to boolean" do
        source = Source.new %(
          if false != a
            :ok
          end
        )
        subject.catch(source).should_not be_valid
      end

      it "fails if there is case comparison to boolean" do
        source = Source.new %(
          true === a
        )
        subject.catch(source).should_not be_valid
      end

      it "reports rule, pos and message" do
        source = Source.new "true != a", "source.cr"
        subject.catch(source).should_not be_valid

        issue = source.issues.first
        issue.rule.should_not be_nil
        issue.location.to_s.should eq "source.cr:1:1"
        issue.end_location.to_s.should eq "source.cr:1:9"
        issue.message.should eq "Comparison to a boolean is pointless"
      end
    end
  end
end
