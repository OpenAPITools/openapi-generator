require "../../../spec_helper"

module Ameba::Rule::Lint
  describe RedundantWithIndex do
    subject = RedundantWithIndex.new

    context "with_index" do
      it "does not report if there is index argument" do
        s = Source.new %(
          collection.each.with_index do |e, i|
            e += i
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is not index argument" do
        s = Source.new %(
          collection.each.with_index do |e|
            e += 1
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "reports if there is underscored index argument" do
        s = Source.new %(
          collection.each.with_index do |e, _|
            e += 1
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "reports if there is no args" do
        s = Source.new %(
          collection.each.with_index do
            puts :nothing
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "does not report if there is no block" do
        s = Source.new %(
          collection.each.with_index
        )
        subject.catch(s).should be_valid
      end

      it "does not report if first argument is underscored" do
        s = Source.new %(
          collection.each.with_index do |_, i|
            puts i
          end
        )
        subject.catch(s).should be_valid
      end

      it "does not report if there are more than 2 args" do
        s = Source.new %(
          tup.each.with_index do |key, value, index|
            puts i
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports rule, location and message" do
        s = Source.new %(
          def valid?
            collection.each.with_index do |e|
            end
          end
        ), "source.cr"
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.rule.should_not be_nil
        issue.location.to_s.should eq "source.cr:2:19"
        issue.end_location.to_s.should eq "source.cr:2:29"
        issue.message.should eq "Remove redundant with_index"
      end
    end

    context "each_with_index" do
      it "does not report if there is index argument" do
        s = Source.new %(
          collection.each_with_index do |e, i|
            e += i
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is not index argument" do
        s = Source.new %(
          collection.each_with_index do |e|
            e += 1
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "reports if there is underscored index argument" do
        s = Source.new %(
          collection.each_with_index do |e, _|
            e += 1
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "reports if there is no args" do
        s = Source.new %(
          collection.each_with_index do
            puts :nothing
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "does not report if there is no block" do
        s = Source.new %(
          collection.each_with_index(1)
        )
        subject.catch(s).should be_valid
      end

      it "does not report if first argument is underscored" do
        s = Source.new %(
          collection.each_with_index do |_, i|
            puts i
          end
        )
        subject.catch(s).should be_valid
      end

      it "does not report if there are more than 2 args" do
        s = Source.new %(
          tup.each_with_index do |key, value, index|
            puts i
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports rule, location and message" do
        s = Source.new %(
          def valid?
            collection.each_with_index do |e|
            end
          end
        ), "source.cr"
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.rule.should_not be_nil
        issue.location.to_s.should eq "source.cr:2:14"
        issue.end_location.to_s.should eq "source.cr:2:29"
        issue.message.should eq "Use each instead of each_with_index"
      end
    end
  end
end
