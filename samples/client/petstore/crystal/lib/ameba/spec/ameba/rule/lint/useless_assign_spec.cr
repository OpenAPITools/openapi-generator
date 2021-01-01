require "../../../spec_helper"

module Ameba::Rule::Lint
  describe UselessAssign do
    subject = UselessAssign.new

    it "does not report used assigments" do
      s = Source.new %(
        def method
          a = 2
          a
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports a useless assignment in a method" do
      s = Source.new %(
        def method
          a = 2
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports a useless assignment in a proc" do
      s = Source.new %(
        ->() {
          a = 2
        }
      )
      subject.catch(s).should_not be_valid
    end

    it "reports a useless assignment in a block" do
      s = Source.new %(
        def method
          3.times do
            a = 1
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports a useless assignment in a proc inside def" do
      s = Source.new %(
        def method
          ->() {
            a = 2
          }
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "does not report ignored assigments" do
      s = Source.new %(
        payload, _header = decode
        puts payload
      )
      subject.catch(s).should be_valid
    end

    it "reports a useless assignment in a proc inside a block" do
      s = Source.new %(
        def method
          3.times do
            ->() {
              a = 2
            }
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports rule, position and a message" do
      s = Source.new %(
        def method
          a = 2
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:3"
      issue.message.should eq "Useless assignment to variable `a`"
    end

    it "does not report useless assignment of instance var" do
      s = Source.new %(
        class Cls
          def initialize(@name)
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "does not report if assignment used in the inner block scope" do
      s = Source.new %(
        def method
          var = true
          3.times { var = false }
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if assigned is not referenced in the inner block scope" do
      s = Source.new %(
        def method
          var = true
          3.times {}
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if assignment in referenced in inner block" do
      s = Source.new %(
        def method
          two = true

          3.times do
            mutex.synchronize do
              two = 2
            end
          end

          two.should be_true
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if first assignment is useless" do
      s = Source.new %(
        def method
          var = true
          var = false
          var
        end
      )
      subject.catch(s).should_not be_valid
      s.issues.first.location.to_s.should eq ":2:3"
    end

    it "reports if variable reassigned and not used" do
      s = Source.new %(
        def method
          var = true
          var = false
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "does not report if variable used in a condition" do
      s = Source.new %(
        def method
          a = 1
          if a
            nil
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports second assignment as useless" do
      s = Source.new %(
        def method
          a = 1
          a = a + 1
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "does not report if variable is referenced in other assignment" do
      s = Source.new %(
        def method
          if f = get_something
            @f = f
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "does not report if variable is referenced in a setter" do
      s = Source.new %(
        def method
          foo = 2
          table[foo] ||= "bar"
        end
      )
      subject.catch(s).should be_valid
    end

    it "does not report if variable is reassigned but not referenced" do
      s = Source.new %(
        def method
          foo = 1
          puts foo
          foo = 2
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "does not report if variable is referenced in a call" do
      s = Source.new %(
        def method
          if f = FORMATTER
            @formatter = f.new
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "does not report if a setter is invoked with operator assignment" do
      s = Source.new %(
        def method
          obj = {} of Symbol => Int32
          obj[:name] = 3
        end
      )
      subject.catch(s).should be_valid
    end

    context "when transformed" do
      it "does not report if the first arg is transformed and not used" do
        s = Source.new %(
          collection.each do |(a, b)|
            puts b
          end
        )
        subject.catch(s).should be_valid
      end

      it "does not report if the second arg is transformed and not used" do
        s = Source.new %(
          collection.each do |(a, b)|
            puts a
          end
        )
        subject.catch(s).should be_valid
      end

      it "does not report if all transformed args are not used in a block" do
        s = Source.new %(
          collection.each do |(foo, bar), (baz, _qux), index, object|
          end
        )
        subject.catch(s).should be_valid
      end
    end

    it "does not report if global var" do
      s = Source.new %(
        def method
          $1 = 3
        end
      )
      subject.catch(s).should be_valid
    end

    it "does not report if assignment is referenced in a proc" do
      s = Source.new %(
        def method
          called = false
          ->() { called = true }
          called
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if variable is shadowed in inner scope" do
      s = Source.new %(
        def method
          i = 1
          3.times do |i|
            i + 1
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "does not report if parameter is referenced after the branch" do
      s = Source.new %(
        def method(param)
          3.times do
            param = 3
          end
          param
        end
      )
      subject.catch(s).should be_valid
    end

    context "op assigns" do
      it "does not report if variable is referenced below the op assign" do
        s = Source.new %(
          def method
            a = 1
            a += 1
            a
          end
        )
        subject.catch(s).should be_valid
      end

      it "does not report if variable is referenced in op assign few times" do
        s = Source.new %(
          def method
            a = 1
            a += 1
            a += 1
            a = a + 1
            a
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if variable is not referenced below the op assign" do
        s = Source.new %(
          def method
            a = 1
            a += 1
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "reports rule, location and a message" do
        s = Source.new %(
          def method
            b = 2
            a = 3
            a += 1
          end
        ), "source.cr"
        subject.catch(s).should_not be_valid

        issue = s.issues.last
        issue.rule.should_not be_nil
        issue.location.to_s.should eq "source.cr:4:3"
        issue.message.should eq "Useless assignment to variable `a`"
      end
    end

    context "multi assigns" do
      it "does not report if all assigns are referenced" do
        s = Source.new %(
          def method
            a, b = {1, 2}
            a + b
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if one assign is not referenced" do
        s = Source.new %(
          def method
            a, b = {1, 2}
            a
          end
        )
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.location.to_s.should eq ":2:6"
        issue.message.should eq "Useless assignment to variable `b`"
      end

      it "reports if both assigns are reassigned and useless" do
        s = Source.new %(
          def method
            a, b = {1, 2}
            a, b = {3, 4}
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "reports if both assigns are not referenced" do
        s = Source.new %(
          def method
            a, b = {1, 2}
          end
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":2:3"
        issue.message.should eq "Useless assignment to variable `a`"

        issue = s.issues.last
        issue.location.to_s.should eq ":2:6"
        issue.message.should eq "Useless assignment to variable `b`"
      end
    end

    context "top level" do
      it "reports if assignment is not referenced" do
        s = Source.new %(
          a = 1
          a = 2
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":1:1"
        s.issues.last.location.to_s.should eq ":2:1"
      end

      it "doesn't report if assignments are referenced" do
        s = Source.new %(
          a = 1
          a += 1
          a

          b, c = {1, 2}
          b
          c
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if assignment is captured by block" do
        s = Source.new %(
          a = 1

          3.times do
            a = 2
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if assignment initialized and captured by block" do
        s = Source.new %(
          a : String? = nil

          1.times do
            a = "Fotis"
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if this is a record declaration" do
        s = Source.new %(
          record Foo, foo = "foo"
        )
        subject.catch(s).should be_valid
      end

      it "does not report if assignment is referenced after the record declaration" do
        s = Source.new %(
          foo = 2
          record Bar, foo = 3 # foo = 3 is not parsed as assignment
          puts foo
        )
        subject.catch(s).should be_valid
      end

      it "reports if assignment is not referenced after the record declaration" do
        s = Source.new %(
          foo = 2
          record Bar, foo = 3
        ), "source.cr"
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 1

        issue = s.issues.first
        issue.location.to_s.should eq "source.cr:1:1"
        issue.message.should eq "Useless assignment to variable `foo`"
      end
    end

    context "branching" do
      context "if-then-else" do
        it "doesn't report if assignment is consumed by branches" do
          s = Source.new %(
            def method
              a = 0
              if something
                a = 1
              else
                a = 2
              end
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "doesn't report if assignment is in one branch" do
          s = Source.new %(
            def method
              a = 0
              if something
                a = 1
              else
                nil
              end
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "doesn't report if assignment is in one line branch" do
          s = Source.new %(
            def method
              a = 0
              a = 1 if something
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if assignment is useless in the branch" do
          s = Source.new %(
            def method(a)
              if a
                a = 2
              end
            end
          )
          subject.catch(s).should_not be_valid
        end

        it "reports if only last assignment is referenced in a branch" do
          s = Source.new %(
            def method(a)
              a = 1
              if a
                a = 2
                a = 3
              end
              a
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":4:5"
        end

        it "does not report of assignments are referenced in all branches" do
          s = Source.new %(
            def method
              if matches
                matches = owner.lookup_matches signature
              else
                matches = owner.lookup_matches signature
              end

              matches
            end
          )
          subject.catch(s).should be_valid
        end

        it "does not report referenced assignments in inner branches" do
          s = Source.new %(
            def method
              has_newline = false

              if something
                do_something unless false
                has_newline = false
              else
                do_something if true
                has_newline = true
              end

              has_newline
            end
          )
          subject.catch(s).should be_valid
        end
      end

      context "unless-then-else" do
        it "doesn't report if assignment is consumed by branches" do
          s = Source.new %(
            def method
              a = 0
              unless something
                a = 1
              else
                a = 2
              end
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if there is a useless assignment in a branch" do
          s = Source.new %(
            def method
              a = 0
              unless something
                a = 1
                a = 2
              else
                a = 2
              end
              a
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":4:5"
        end
      end

      context "case" do
        it "does not report if assignment is referenced" do
          s = Source.new %(
            def method(a)
              case a
              when /foo/
                a = 1
              when /bar/
                a = 2
              end
              puts a
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if assignment is useless" do
          s = Source.new %(
            def method(a)
              case a
              when /foo/
                a = 1
              when /bar/
                a = 2
              end
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 2
          s.issues.first.location.to_s.should eq ":4:5"
          s.issues.last.location.to_s.should eq ":6:5"
        end

        it "doesn't report if assignment is referenced in cond" do
          s = Source.new %(
            def method
              a = 2
              case a
              when /foo/
              end
            end
          )
          subject.catch(s).should be_valid
        end
      end

      context "binary operator" do
        it "does not report if assignment is referenced" do
          s = Source.new %(
            def method(a)
              (a = 1) && (b = 1)
              a + b
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if assignment is useless" do
          s = Source.new %(
            def method(a)
              (a = 1) || (b = 1)
              a
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":2:15"
        end
      end

      context "while" do
        it "does not report if assignment is referenced" do
          s = Source.new %(
            def method(a)
              while a < 10
                a = a + 1
              end
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if assignment is useless" do
          s = Source.new %(
            def method(a)
              while a < 10
                b = a
              end
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":3:5"
        end

        it "does not report if assignment is referenced in a loop" do
          s = Source.new %(
            def method
              a = 3
              result = 0

              while result < 10
                result += a
                a = a + 1
              end
              result
            end
          )
          subject.catch(s).should be_valid
        end

        it "does not report if assignment is referenced as param in a loop" do
          s = Source.new %(
            def method(a)
              result = 0

              while result < 10
                result += a
                a = a + 1
              end
              result
            end
          )
          subject.catch(s).should be_valid
        end

        it "does not report if assignment is referenced in loop and inner branch" do
          s = Source.new %(
            def method(a)
              result = 0

              while result < 10
                result += a
                if result > 0
                  a = a + 1
                else
                  a = 3
                end
              end
              result
            end
          )
          subject.catch(s).should be_valid
        end

        it "works properly if there is branch with blank node" do
          s = Source.new %(
            def visit
              count = 0
              while true
                break if count == 1
                case something
                when :any
                else
                  :anything_else
                end
                count += 1
              end
            end
          )
          subject.catch(s).should be_valid
        end
      end

      context "until" do
        it "does not report if assignment is referenced" do
          s = Source.new %(
            def method(a)
              until a > 10
                a = a + 1
              end
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if assignment is useless" do
          s = Source.new %(
            def method(a)
              until a > 10
                b = a + 1
              end
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":3:5"
        end
      end

      context "exception handler" do
        it "does not report if assignment is referenced in body" do
          s = Source.new %(
            def method(a)
              a = 2
            rescue
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "doesn't report if assignment is referenced in ensure" do
          s = Source.new %(
            def method(a)
              a = 2
            ensure
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "doesn't report if assignment is referenced in else" do
          s = Source.new %(
            def method(a)
              a = 2
            rescue
            else
              a
            end
          )
          subject.catch(s).should be_valid
        end

        it "reports if assignment is useless" do
          s = Source.new %(
            def method(a)
            rescue
              a = 2
            end
          )
          subject.catch(s).should_not be_valid
          s.issues.size.should eq 1
          s.issues.first.location.to_s.should eq ":3:3"
        end
      end
    end

    context "typeof" do
      it "reports useless assigments in typeof" do
        s = Source.new %(
          typeof(begin
            foo = 1
            bar = 2
          end)
        )
        subject.catch(s).should_not be_valid
        s.issues.size.should eq 2
        s.issues.first.location.to_s.should eq ":2:3"
        s.issues.first.message.should eq "Useless assignment to variable `foo`"

        s.issues.last.location.to_s.should eq ":3:3"
        s.issues.last.message.should eq "Useless assignment to variable `bar`"
      end
    end

    context "macro" do
      it "doesn't report if assignment is referenced in macro" do
        s = Source.new %(
          def method
            a = 2
            {% if flag?(:bits64) %}
              a.to_s
            {% else %}
              a
            {% end %}
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report referenced assignments in macro literal" do
        s = Source.new %(
          def method
            a = 2
            {% if flag?(:bits64) %}
              a = 3
            {% else %}
              a = 4
            {% end %}
            puts a
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if assignment is referenced in macro def" do
        s = Source.new %(
          macro macro_call
            puts x
          end

          def foo
            x = 1
            macro_call
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if assignment is referenced in a macro below" do
        s = Source.new %(
          class Foo
            def foo
              a = 1
              macro_call
            end

            macro macro_call
              puts a
            end
          end
        )
        subject.catch(s).should be_valid
      end
    end

    context "uninitialized" do
      it "reports if uninitialized assignment is not referenced at a top level" do
        s = Source.new %(
          a = uninitialized U
        )
        subject.catch(s).should_not be_valid
      end

      it "reports if uninitialized assignment is not referenced in a method" do
        s = Source.new %(
          def foo
            a = uninitialized U
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report if uninitialized assignment is referenced" do
        s = Source.new %(
          def foo
            a = uninitialized U
            a
          end
        )
        subject.catch(s).should be_valid
      end
    end
  end
end
