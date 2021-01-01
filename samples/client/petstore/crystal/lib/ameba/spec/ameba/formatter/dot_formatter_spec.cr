require "../../spec_helper"

module Ameba::Formatter
  describe DotFormatter do
    output = IO::Memory.new
    subject = DotFormatter.new output

    describe "#started" do
      it "writes started message" do
        subject.started [Source.new ""]
        output.to_s.should eq "Inspecting 1 file.\n\n"
      end
    end

    describe "#source_finished" do
      it "writes valid source" do
        subject.source_finished Source.new ""
        output.to_s.should contain "."
      end

      it "writes invalid source" do
        s = Source.new ""
        s.add_issue DummyRule.new, Crystal::Nop.new, "message"
        subject.source_finished s
        output.to_s.should contain "F"
      end
    end

    describe "#finished" do
      it "writes a final message" do
        subject.finished [Source.new ""]
        output.to_s.should contain "1 inspected, 0 failures."
      end

      it "writes the elapsed time" do
        subject.finished [Source.new ""]
        output.to_s.should contain "Finished in"
      end

      context "when issues found" do
        it "writes each issue" do
          s = Source.new("").tap do |source|
            source.add_issue(DummyRule.new, {1, 1}, "DummyRuleError")
            source.add_issue(NamedRule.new, {1, 2}, "NamedRuleError")
          end
          subject.finished [s]
          log = output.to_s
          log.should contain "1 inspected, 2 failures."
          log.should contain "DummyRuleError"
          log.should contain "NamedRuleError"
        end

        it "writes affected code by default" do
          output.clear
          s = Source.new(%(
            a = 22
            puts a
          )).tap do |source|
            source.add_issue(DummyRule.new, {1, 5}, "DummyRuleError")
          end
          subject.finished [s]
          log = output.to_s
          log.should contain "> a = 22"
          log.should contain "      \e[33m^\e[0m"
        end

        it "writes severity" do
          output.clear
          s = Source.new(%(
            a = 22
            puts a
          )).tap do |source|
            source.add_issue(DummyRule.new, {1, 5}, "DummyRuleError")
          end
          subject.finished [s]
          log = output.to_s
          log.should contain "[C]"
        end

        it "doesn't write affected code if it is disabled" do
          output.clear
          s = Source.new(%(
            a = 22
            puts a
          )).tap do |source|
            source.add_issue(DummyRule.new, {1, 5}, "DummyRuleError")
          end

          formatter = DotFormatter.new output
          formatter.config[:without_affected_code] = true
          formatter.finished [s]
          log = output.to_s
          log.should_not contain "> a = 22"
          log.should_not contain "      \e[33m^\e[0m"
        end

        it "does not write disabled issues" do
          s = Source.new ""
          s.add_issue(DummyRule.new, location: {1, 1},
            message: "DummyRuleError", status: :disabled)
          subject.finished [s]
          output.to_s.should contain "1 inspected, 0 failures."
        end
      end
    end
  end
end
