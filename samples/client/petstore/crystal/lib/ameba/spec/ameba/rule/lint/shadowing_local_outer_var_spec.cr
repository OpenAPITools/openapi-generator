require "../../../spec_helper"

module Ameba::Rule::Lint
  describe ShadowingOuterLocalVar do
    subject = ShadowingOuterLocalVar.new

    it "doesn't report if there is no shadowing" do
      source = Source.new %(
        def some_method
          foo = 1

          3.times do |bar|
            bar
          end

          -> (baz : Int32) {}

          -> (bar : String) {}
        end
      )

      subject.catch(source).should be_valid
    end

    it "reports if there is a shadowing in a block" do
      source = Source.new %(
        def some_method
          foo = 1

          3.times do |foo|
          end
        end
      )
      subject.catch(source).should_not be_valid
    end

    it "does not report outer vars declared below shadowed block" do
      source = Source.new %(
        methods = klass.methods.select { |m| m.annotation(MyAnn) }
        m = methods.last
      )
      subject.catch(source).should be_valid
    end

    it "reports if there is a shadowing in a proc" do
      source = Source.new %(
        def some_method
          foo = 1

          -> (foo : Int32) {}
        end
      )
      subject.catch(source).should_not be_valid
    end

    it "reports if there is a shadowing in an inner scope" do
      source = Source.new %(
        def foo
          foo = 1

          3.times do |i|
            3.times { |foo| foo }
          end
        end
      )
      subject.catch(source).should_not be_valid
    end

    it "reports if variable is shadowed twice" do
      source = Source.new %(
        foo = 1

        3.times do |foo|
          -> (foo : Int32) { foo + 1 }
        end
      )
      subject.catch(source).should_not be_valid

      source.issues.size.should eq 2
    end

    it "reports if a splat block argument shadows local var" do
      source = Source.new %(
        foo = 1

        3.times do |*foo|
        end
      )
      subject.catch(source).should_not be_valid
    end

    it "reports if a &block argument is shadowed" do
      source = Source.new %(
        def method_with_block(a, &block)
          3.times do |block|
          end
        end
      )
      subject.catch(source).should_not be_valid
      source.issues.first.message.should eq "Shadowing outer local variable `block`"
    end

    it "reports if there are multiple args and one shadows local var" do
      source = Source.new %(
        foo = 1
        [1, 2, 3].each_with_index do |i, foo|
          i + foo
        end
      )
      subject.catch(source).should_not be_valid
      source.issues.first.message.should eq "Shadowing outer local variable `foo`"
    end

    it "doesn't report if an outer var is reassigned in a block" do
      source = Source.new %(
        def foo
          foo = 1
          3.times do |i|
            foo = 2
          end
        end
      )
      subject.catch(source).should be_valid
    end

    it "doesn't report if an argument is a black hole '_'" do
      source = Source.new %(
        _ = 1
        3.times do |_|
        end
      )
      subject.catch(source).should be_valid
    end

    it "doesn't report if it shadows record type declaration" do
      source = Source.new %(
        class FooBar
          record Foo, index : String

          def bar
            3.times do |index|
            end
          end
        end
      )
      subject.catch(source).should be_valid
    end

    it "doesn't report if it shadows throwaway arguments" do
      source = Source.new %(
        data = [{1, "a"}, {2, "b"}, {3, "c"}]

        data.each do |_, string|
          data.each do |number, _|
            puts string, number
          end
        end
      )
      subject.catch(source).should be_valid
    end

    it "does not report if argument shadows an ivar assignment" do
      s = Source.new %(
        def bar(@foo)
          @foo.try do |foo|
          end
        end
      )
      subject.catch(s).should be_valid
    end

    it "reports rule, location and message" do
      source = Source.new %(
        foo = 1
        3.times { |foo| foo + 1 }
      ), "source.cr"
      subject.catch(source).should_not be_valid

      issue = source.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:12"
      issue.end_location.should be_nil
      issue.message.should eq "Shadowing outer local variable `foo`"
    end

    context "macro" do
      it "does not report shadowed vars in outer scope" do
        source = Source.new %(
          macro included
            def foo
              {% for ivar in instance_vars %}
                {% ann = ivar.annotation(Name) %}
              {% end %}
            end

            def bar
              {% instance_vars.reject { |ivar| ivar } %}
            end
          end
        )
        subject.catch(source).should be_valid
      end

      it "does not report shadowed vars in macro withing the same scope" do
        source = Source.new %(
          {% methods = klass.methods.select { |m| m.annotation(MyAnn) } %}

          {% for m, m_idx in methods %}
            {% if d = m.annotation(MyAnn) %}
              {% d %}
            {% end %}
          {% end %}
        )
        subject.catch(source).should be_valid
      end

      it "does not report shadowed vars withing nested macro" do
        source = Source.new %(
          module Foo
            macro included
              def foo
                {% for ann in instance_vars %}
                  {% pos_args = ann.args.empty? ? "Tuple.new".id : ann.args %}
                {% end %}
              end

              def bar
                {{@type.instance_vars.map do |ivar|
                    ivar.annotations(Name).each do |ann|
                      puts ann.args
                    end
                  end}}
              end
            end
          end
        )
        subject.catch(source).should be_valid
      end
    end
  end
end
