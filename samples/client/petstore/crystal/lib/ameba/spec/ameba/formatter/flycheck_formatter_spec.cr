require "../../spec_helper"

private def flycheck
  output = IO::Memory.new
  Ameba::Formatter::FlycheckFormatter.new output
end

module Ameba::Formatter
  describe FlycheckFormatter do
    context "problems not found" do
      it "reports nothing" do
        subject = flycheck
        subject.source_finished Source.new ""
        subject.output.to_s.empty?.should be_true
      end
    end

    context "when problems found" do
      it "reports an issue" do
        s = Source.new "a = 1", "source.cr"
        s.add_issue DummyRule.new, {1, 2}, "message"
        subject = flycheck
        subject.source_finished s
        subject.output.to_s.should eq(
          "source.cr:1:2: C: [#{DummyRule.rule_name}] message\n"
        )
      end

      it "properly reports multi-line message" do
        s = Source.new "a = 1", "source.cr"
        s.add_issue DummyRule.new, {1, 2}, "multi\nline"
        subject = flycheck
        subject.source_finished s
        subject.output.to_s.should eq(
          "source.cr:1:2: C: [#{DummyRule.rule_name}] multi line\n"
        )
      end

      it "reports nothing if location was not set" do
        s = Source.new "a = 1", "source.cr"
        s.add_issue DummyRule.new, Crystal::Nop.new, "message"
        subject = flycheck
        subject.source_finished s
        subject.output.to_s.should eq ""
      end
    end
  end
end
