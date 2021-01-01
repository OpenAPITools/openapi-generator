require "../../../spec_helper"

module Ameba::Rule::Style
  subject = PredicateName.new

  describe PredicateName do
    it "passes if predicate name is correct" do
      s = Source.new %q(
        def valid?(x)
        end

        class Image
          def picture?(x)
          end
        end

        def allow_this_picture?
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if predicate name is wrong" do
      s = Source.new %q(
        def is_valid?(x)
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, pos and message" do
      s = Source.new %q(
        class Image
          def has_picture?(x)
            true
          end
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:3"
      issue.end_location.to_s.should eq "source.cr:4:5"
      issue.message.should eq(
        "Favour method name 'picture?' over 'has_picture?'")
    end

    it "ignores if alternative name isn't valid syntax" do
      s = Source.new %q(
        class Image
          def is_404?(x)
            true
          end
        end
      )
      subject.catch(s).should be_valid
    end
  end
end
