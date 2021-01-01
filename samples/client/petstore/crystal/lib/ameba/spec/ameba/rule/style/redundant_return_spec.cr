require "../../../spec_helper"

module Ameba::Rule::Style
  subject = RedundantReturn.new

  describe RedundantReturn do
    it "does not report if there is no return" do
      s = Source.new %(
        def inc(a)
          a + 1
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is redundant return in method body" do
      s = Source.new %(
        def inc(a)
          return a + 1
        end
      )
      subject.catch(s).should_not be_valid
      s.issues.size.should eq 1
      s.issues.first.location.to_s.should eq ":2:3"
    end

    it "doesn't report if it returns tuple literal" do
      s = Source.new %(
        def foo(a)
          return a, a + 2
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if there are other expressions after control flow" do
      s = Source.new %(
        def method(a)
          case a
          when true then return true
          when .nil? then return :nil
          end
          false
        rescue
          nil
        end
      )
      subject.catch(s).should be_valid
    end

    context "if" do
      it "doesn't report if there is return in if branch" do
        s = Source.new %(
          def inc(a)
            return a + 1 if a > 0
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there are returns in if/else branch" do
        s = Source.new %(
          def inc(a)
            do_something(a)
            if a > 0
              return :positive
            else
              return :negative
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":4:5"
        s.issues.last.location.to_s.should eq ":6:5"
      end
    end

    context "unless" do
      it "doesn't report if there is return in unless branch" do
        s = Source.new %(
          def inc(a)
            return a + 1 unless a > 0
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there are returns in unless/else branch" do
        s = Source.new %(
          def inc(a)
            do_something(a)
            unless a < 0
              return :positive
            else
              return :negative
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":4:5"
        s.issues.last.location.to_s.should eq ":6:5"
      end
    end

    context "binary op" do
      it "doesn't report if there is no return in the right binary op node" do
        s = Source.new %(
          def can_create?(a)
            valid? && a > 0
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is return in the right binary op node" do
        s = Source.new %(
          def can_create?(a)
            valid? && return a > 0
          end
        )
        subject.catch(s).should_not be_valid
      end
    end

    context "case" do
      it "reports if there are returns in whens" do
        s = Source.new %(
          def foo(a)
            case a
            when .nil?
              puts "blah"
              return nil
            when .blank?
              return ""
            when true
              true
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":5:5"
        s.issues.last.location.to_s.should eq ":7:5"
      end

      it "reports if there is return in else" do
        s = Source.new %(
          def foo(a)
            do_something_with(a)

            case a
            when true
              true
            else
              return false
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 1
        s.issues.first.location.to_s.should eq ":8:5"
      end
    end

    context "exception handler" do
      it "reports if there are returns in body" do
        s = Source.new %(
          def foo(a)
            return true
          rescue
            false
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 1
        s.issues.first.location.to_s.should eq ":2:3"
      end

      it "reports if there are returns in rescues" do
        s = Source.new %(
          def foo(a)
            true
          rescue ArgumentError
            return false
          rescue RuntimeError
            ""
          rescue Exception
            return nil
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":4:3"
        s.issues.last.location.to_s.should eq ":8:3"
      end

      it "reports if there are returns in else" do
        s = Source.new %(
          def foo(a)
            true
          rescue Exception
            nil
          else
            puts "else branch"
            return :bar
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 1
        s.issues.first.location.to_s.should eq ":7:3"
      end
    end

    context "properties" do
      context "#allow_multi_return=" do
        it "allows multi returns by default" do
          s = Source.new %(
            def method(a, b)
              return a, b
            end
          )
          subject.catch(s).should be_valid
        end

        it "allows to configure multi returns" do
          s = Source.new %(
            def method(a, b)
              return a, b
            end
          )
          rule = Rule::Style::RedundantReturn.new
          rule.allow_multi_return = false
          rule.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":2:3"
        end
      end

      context "#allow_empty_return" do
        it "allows empty returns by default" do
          s = Source.new %(
            def method
              return
            end
          )
          subject.catch(s).should be_valid
        end

        it "allows to configure empty returns" do
          s = Source.new %(
            def method
              return
            end
          )
          rule = Rule::Style::RedundantReturn.new
          rule.allow_empty_return = false
          rule.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":2:3"
        end
      end
    end
  end
end
