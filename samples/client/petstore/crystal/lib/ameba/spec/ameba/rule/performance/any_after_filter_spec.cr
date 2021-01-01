require "../../../spec_helper"

module Ameba::Rule::Performance
  subject = AnyAfterFilter.new

  describe AnyAfterFilter do
    it "passes if there is no potential performance improvements" do
      source = Source.new %(
        [1, 2, 3].select { |e| e > 1 }.any?(&.zero?)
        [1, 2, 3].reject { |e| e > 1 }.any?(&.zero?)
        [1, 2, 3].select { |e| e > 1 }
        [1, 2, 3].reject { |e| e > 1 }
        [1, 2, 3].any? { |e| e > 1 }
      )
      subject.catch(source).should be_valid
    end

    it "reports if there is select followed by any? without a block" do
      source = Source.new %(
        [1, 2, 3].select { |e| e > 2 }.any?
      )
      subject.catch(source).should_not be_valid
    end

    it "reports if there is reject followed by any? without a block" do
      source = Source.new %(
        [1, 2, 3].reject { |e| e > 2 }.any?
      )
      subject.catch(source).should_not be_valid
    end

    it "does not report if any? calls contains a block" do
      source = Source.new %(
        [1, 2, 3].select { |e| e > 2 }.any?(&.zero?)
        [1, 2, 3].reject { |e| e > 2 }.any?(&.zero?)
      )
      subject.catch(source).should be_valid
    end

    context "properties" do
      it "allows to configure object_call_names" do
        source = Source.new %(
          [1, 2, 3].reject { |e| e > 2 }.any?
        )
        rule = Rule::Performance::AnyAfterFilter.new
        rule.filter_names = %w(select)
        rule.catch(source).should be_valid
      end
    end

    context "macro" do
      it "reports in macro scope" do
        source = Source.new %(
          {{ [1, 2, 3].reject { |e| e > 2  }.any? }}
        )
        subject.catch(source).should_not be_valid
      end
    end

    it "reports rule, pos and message" do
      s = Source.new %(
        [1, 2, 3].reject { |e| e > 2 }.any?
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first

      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:11"
      issue.end_location.to_s.should eq "source.cr:1:36"
      issue.message.should eq "Use `any? {...}` instead of `reject {...}.any?`"
    end
  end
end
