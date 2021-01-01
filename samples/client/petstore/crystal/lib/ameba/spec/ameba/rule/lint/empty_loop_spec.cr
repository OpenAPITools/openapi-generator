require "../../../spec_helper"

module Ameba::Rule::Lint
  describe EmptyLoop do
    subject = EmptyLoop.new

    it "does not report if there are not empty loops" do
      s = Source.new %(
        a = 1

        while a < 10
          a += 1
        end

        until a == 10
         a += 1
        end

        loop do
          a += 1
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is an empty while loop" do
      s = Source.new %(
        a = 1
        while true
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if while loop has non-literals in cond block" do
      s = Source.new %(
        a = 1
        while a = gets.to_s
          # nothing here
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is an empty until loop" do
      s = Source.new %(
        do_something
        until false
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if until loop has non-literals in cond block" do
      s = Source.new %(
        until socket_open?
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if there an empty loop" do
      s = Source.new %(
        a = 1
        loop do

        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, message and location" do
      s = Source.new %(
        a = 1
        loop do
          # comment goes here
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:1"
      issue.end_location.to_s.should eq "source.cr:4:3"
      issue.message.should eq EmptyLoop::MSG
    end
  end
end
