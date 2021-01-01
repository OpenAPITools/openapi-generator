require "../../../spec_helper"

module Ameba::Rule::Lint
  describe ShadowedArgument do
    subject = ShadowedArgument.new

    it "doesn't report if there is not a shadowed argument" do
      s = Source.new %(
        def foo(bar)
          baz = 1
        end

        3.times do |i|
          a = 1
        end

        proc = -> (a : Int32) {
          b = 2
        }
      )
      subject.catch(s).should be_valid
    end

    it "reports if there is a shadowed method argument" do
      s = Source.new %(
        def foo(bar)
          bar = 1
          bar
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports if there is a shadowed block argument" do
      s = Source.new %(
        3.times do |i|
          i = 2
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports if there is a shadowed proc argument" do
      s = Source.new %(
        ->(x : Int32) {
          x = 20
          x
        }
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if the argument is referenced before the assignment" do
      s = Source.new %(
        def foo(bar)
          bar
          bar = 1
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if the argument is conditionally reassigned" do
      s = Source.new %(
        def foo(bar = nil)
          bar ||= true
          bar
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if the op assign is followed by another assignment" do
      s = Source.new %(
        def foo(bar)
          bar ||= 3
          bar = 43
          bar
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if the shadowing assignment is followed by op assign" do
      s = Source.new %(
        def foo(bar)
          bar = 42
          bar ||= 43
          bar
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if the argument is unused" do
      s = Source.new %(
        def foo(bar)
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if the argument is shadowed before super" do
      s = Source.new %(
        def foo(bar)
          bar = 1
          super
        end
      )
      subject.catch(s).should_not be_valid
    end

    context "branch" do
      it "doesn't report if the argument is not shadowed in a condition" do
        s = Source.new %(
          def foo(bar, baz)
            bar = 1 if baz
            bar
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if the argument is shadowed after the condition" do
        s = Source.new %(
          def foo(foo)
            if something
              foo = 42
            end
            foo = 43
            foo
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report if the argument is conditionally assigned in a branch" do
        s = Source.new %(
          def foo(bar)
            if something
              bar ||= 22
            end
            bar
          end
        )
        subject.catch(s).should be_valid
      end
    end

    it "reports rule, location and message" do
      s = Source.new %(
        def foo(bar)
          bar = 22
          bar
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:3"
      issue.end_location.to_s.should eq "source.cr:2:10"
      issue.message.should eq "Argument `bar` is assigned before it is used"
    end
  end
end
