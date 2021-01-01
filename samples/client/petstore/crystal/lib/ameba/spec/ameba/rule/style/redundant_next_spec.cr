require "../../../spec_helper"

module Ameba::Rule::Style
  subject = RedundantNext.new

  describe RedundantNext do
    it "does not report if there is no redundant next" do
      s = Source.new %(
        array.map { |x| x + 1 }
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is redundant next with argument in the block" do
      s = Source.new %(
        block do |v|
          next v + 1
        end
      )
      subject.catch(s).should_not be_valid
    end

    context "if" do
      it "doesn't report if there is not redundant next in if branch" do
        s = Source.new %(
          block do |v|
            next if v > 10
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is redundant next in if/else branch" do
        s = Source.new %(
          block do |a|
            if a > 0
              next a + 1
            else
              next a + 2
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":3:5"
        s.issues.last.location.to_s.should eq ":5:5"
      end
    end

    context "unless" do
      it "doesn't report if there is no redundant next in unless branch" do
        s = Source.new %(
          block do |v|
            next unless v > 10
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is redundant next in unless/else branch" do
        s = Source.new %(
          block do |a|
            unless a > 0
              next a + 1
            else
              next a + 2
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":3:5"
        s.issues.last.location.to_s.should eq ":5:5"
      end
    end

    context "expressions" do
      it "doesn't report if there is no redundant next in expressions" do
        s = Source.new %(
          block do |v|
            a = 1
            a + v
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is redundant next in expressions" do
        s = Source.new %(
          block do |a|
            a = 1
            next a
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 1
        s.issues.first.location.to_s.should eq ":3:3"
      end
    end

    context "binary-op" do
      it "doesn't report if there is no redundant next in binary op" do
        s = Source.new %(
          block do |v|
            a && v
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is redundant next in binary op" do
        s = Source.new %(
          block do |a|
            a && next a
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 1
        s.issues.first.location.to_s.should eq ":2:8"
      end
    end

    context "expception handler" do
      it "doesn't report if there is no redundant next in exception handler" do
        s = Source.new %(
          block do |v|
            v + 1
          rescue e
            next v if v > 0
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is redundant next in exception handler" do
        s = Source.new %(
          block do |a|
            next a + 1
          rescue ArgumentError
            next a + 2
          rescue Exception
            a + 2
            next a
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 3
        s.issues[0].location.to_s.should eq ":2:3"
        s.issues[1].location.to_s.should eq ":4:3"
        s.issues[2].location.to_s.should eq ":7:3"
      end
    end

    it "reports correct rule, error message and position" do
      s = Source.new %(
        block do |v|
          next v + 1
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:3"
      issue.end_location.to_s.should eq "source.cr:2:12"
      issue.message.should eq "Redundant `next` detected"
    end

    context "properties" do
      context "#allow_multi_next=" do
        it "allows multi next statements by default" do
          s = Source.new %(
            block do |a, b|
              next a, b
            end
          )
          subject.catch(s).should be_valid
        end

        it "allows to configure multi next statements" do
          s = Source.new %(
            block do |a, b|
              next a, b
            end
          )
          rule = Rule::Style::RedundantNext.new
          rule.allow_multi_next = false
          rule.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":2:3"
        end
      end

      context "#allow_empty_next" do
        it "allows empty next statements by default" do
          s = Source.new %(
            block do
              next
            end
          )
          subject.catch(s).should be_valid
        end

        it "allows to configure empty next statements" do
          s = Source.new %(
            block do
              next
            end
          )
          rule = Rule::Style::RedundantNext.new
          rule.allow_empty_next = false
          rule.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":2:3"
        end
      end
    end
  end
end
