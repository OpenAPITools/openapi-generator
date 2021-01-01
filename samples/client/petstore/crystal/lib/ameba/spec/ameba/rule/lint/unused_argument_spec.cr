require "../../../spec_helper"

module Ameba::Rule::Lint
  subject = UnusedArgument.new
  subject.ignore_defs = false

  describe UnusedArgument do
    it "doesn't report if arguments are used" do
      s = Source.new %(
        def method(a, b, c)
          a + b + c
        end

        3.times do |i|
          i + 1
        end

        ->(i : Int32) { i + 1 }
      )
      subject.catch(s).should be_valid
    end

    it "reports if method argument is unused" do
      s = Source.new %(
        def method(a, b, c)
          a + b
        end
      )
      subject.catch(s).should_not be_valid
      s.issues.first.message.should eq "Unused argument `c`. If it's necessary, use `_c` " \
                                       "as an argument name to indicate that it won't be used."
    end

    it "reports if block argument is unused" do
      s = Source.new %(
        [1,2].each_with_index do |a, i|
          a
        end
      )
      subject.catch(s).should_not be_valid
      s.issues.first.message.should eq "Unused argument `i`. If it's necessary, use `_` " \
                                       "as an argument name to indicate that it won't be used."
    end

    it "reports if proc argument is unused" do
      s = Source.new %(
        -> (a : Int32, b : String) do
          a = a + 1
        end
      )
      subject.catch(s).should_not be_valid
      s.issues.first.message.should eq "Unused argument `b`. If it's necessary, use `_b` " \
                                       "as an argument name to indicate that it won't be used."
    end

    it "reports multiple unused args" do
      s = Source.new %(
        def method(a, b, c)
          nil
        end
      )
      subject.catch(s).should_not be_valid
      s.issues[0].message.should eq "Unused argument `a`. If it's necessary, use `_a` " \
                                    "as an argument name to indicate that it won't be used."
      s.issues[1].message.should eq "Unused argument `b`. If it's necessary, use `_b` " \
                                    "as an argument name to indicate that it won't be used."
      s.issues[2].message.should eq "Unused argument `c`. If it's necessary, use `_c` " \
                                    "as an argument name to indicate that it won't be used."
    end

    it "doesn't report if it is an instance var argument" do
      s = Source.new %(
        class A
          def method(@name)
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if a typed argument is used" do
      s = Source.new %(
        def method(x : Int32)
          3.times do
            puts x
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if an argument with default value is used" do
      s = Source.new %(
        def method(x = 1)
          puts x
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if argument starts with a _" do
      s = Source.new %(
        def method(_x)
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if it is a block and used" do
      s = Source.new %(
        def method(&block)
          block.call
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports if block arg is not used" do
      s = Source.new %(
        def method(&block)
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "reports if unused and there is yield" do
      s = Source.new %(
        def method(&block)
          yield 1
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if variable is referenced implicitly" do
      s = Source.new %(
        class Bar < Foo
          def method(a, b)
            super
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if arg if referenced in case" do
      s = Source.new %(
        def foo(a)
          case a
          when /foo/
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "doesn't report if enum in a record" do
      s = Source.new %(
        class Class
          record Record do
            enum Enum
              CONSTANT
            end
          end
        end
      )
      subject.catch(s).should be_valid
    end

    context "super" do
      it "reports if variable is not referenced implicitly by super" do
        s = Source.new %(
          class Bar < Foo
            def method(a, b)
              super a
            end
          end
        )
        subject.catch(s).should_not be_valid
        s.issues.first.message.should eq "Unused argument `b`. If it's necessary, use `_b` " \
                                         "as an argument name to indicate that it won't be used."
      end

      it "reports rule, location and message" do
        s = Source.new %(
          def method(a)
          end
        ), "source.cr"
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.rule.should_not be_nil
        issue.message.should eq "Unused argument `a`. If it's necessary, use `_a` " \
                                "as an argument name to indicate that it won't be used."
        issue.location.to_s.should eq "source.cr:1:12"
      end
    end

    context "macro" do
      it "doesn't report if it is a used macro argument" do
        s = Source.new %(
          macro my_macro(arg)
            {% arg %}
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if it is a used macro block argument" do
        s = Source.new %(
          macro my_macro(&block)
            {% block %}
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report used macro args with equal names in record" do
        s = Source.new %(
          record X do
            macro foo(a, b)
              {{a}} + {{b}}
            end

            macro bar(a, b, c)
              {{a}} + {{b}} + {{c}}
            end
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report used args in macro literals" do
        s = Source.new %(
          def print(f : Array(U)) forall U
            f.size.times do |i|
              {% if U == Float64 %}
                puts f[i].round(3)
              {% else %}
                puts f[i]
              {% end %}
            end
          end
        )
        subject.catch(s).should be_valid
      end
    end

    context "properties" do
      describe "#ignore_defs" do
        it "lets the rule to ignore def scopes if true" do
          subject.ignore_defs = true
          s = Source.new %(
            def method(a)
            end
          )
          subject.catch(s).should be_valid
        end

        it "lets the rule not to ignore def scopes if false" do
          subject.ignore_defs = false
          s = Source.new %(
            def method(a)
            end
          )
          subject.catch(s).should_not be_valid
        end
      end

      context "#ignore_blocks" do
        it "lets the rule to ignore block scopes if true" do
          subject.ignore_blocks = true
          s = Source.new %(
            3.times { |i| puts "yo!" }
          )
          subject.catch(s).should be_valid
        end

        it "lets the rule not to ignore block scopes if false" do
          subject.ignore_blocks = false
          s = Source.new %(
            3.times { |i| puts "yo!" }
          )
          subject.catch(s).should_not be_valid
        end
      end

      context "#ignore_procs" do
        it "lets the rule to ignore proc scopes if true" do
          subject.ignore_procs = true
          s = Source.new %(
            ->(a : Int32) {}
          )
          subject.catch(s).should be_valid
        end

        it "lets the rule not to ignore proc scopes if false" do
          subject.ignore_procs = false
          s = Source.new %(
            ->(a : Int32) {}
          )
          subject.catch(s).should_not be_valid
        end
      end
    end
  end
end
