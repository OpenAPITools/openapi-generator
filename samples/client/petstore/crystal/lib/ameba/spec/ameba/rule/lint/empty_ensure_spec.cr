require "../../../spec_helper"

module Ameba::Rule::Lint
  describe EmptyEnsure do
    subject = EmptyEnsure.new

    it "passes if there is no empty ensure blocks" do
      s = Source.new %(
        def some_method
          do_some_stuff
        ensure
          do_something_else
        end

        begin
          do_some_stuff
        ensure
          do_something_else
        end

        def method_with_rescue
        rescue
        ensure
          nil
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is an empty ensure in method" do
      s = Source.new %(
        def method
          do_some_stuff
        ensure
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is an empty ensure in a block" do
      s = Source.new %(
        begin
          do_some_stuff
        ensure
          # nothing here
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, pos and message" do
      s = Source.new %(
        begin
          do_some_stuff
        rescue
          do_some_other_stuff
        ensure
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first

      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:3"
      issue.end_location.to_s.should eq "source.cr:6:3"
      issue.message.should eq "Empty `ensure` block detected"
    end
  end
end
