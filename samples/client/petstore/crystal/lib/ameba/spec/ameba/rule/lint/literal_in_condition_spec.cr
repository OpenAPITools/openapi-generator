require "../../../spec_helper"

module Ameba::Rule::Lint
  subject = LiteralInCondition.new

  describe LiteralInCondition do
    it "passes if there is not literals in conditional" do
      s = Source.new %(
        if a == 2
          :ok
        end

        :ok unless b

        case string
        when "a"
          :ok
        when "b"
          :ok
        end

        unless a.nil?
          :ok
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is a predicate in if conditional" do
      s = Source.new %(
        if "string"
          :ok
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is a predicate in unless conditional" do
      s = Source.new %(
        unless true
          :ok
        end
      )
      subject.catch(s).should_not be_valid
    end

    describe "range" do
      it "reports range with literals" do
        s = Source.new %(
          case 1..2
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report range with non-literals" do
        s = Source.new %(
          case (1..a)
          end
        )
        subject.catch(s).should be_valid
      end
    end

    describe "array" do
      it "reports array with literals" do
        s = Source.new %(
          case [1, 2, 3]
          when :array
            :ok
          when :not_array
            :also_ok
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report array with non-literals" do
        s = Source.new %(
          a, b = 1, 2
          case [1, 2, a]
          when :array
            :ok
          when :not_array
            :also_ok
          end
        )
        subject.catch(s).should be_valid
      end
    end

    describe "hash" do
      it "reports hash with literals" do
        s = Source.new %(
          case { "name" => 1, 33 => 'b' }
          when :hash
            :ok
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report hash with non-literals in keys" do
        s = Source.new %(
          case { a => 1, 33 => 'b' }
          when :hash
            :ok
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report hash with non-literals in values" do
        s = Source.new %(
          case { "name" => a, 33 => 'b' }
          when :hash
            :ok
          end
        )
        subject.catch(s).should be_valid
      end
    end

    describe "tuple" do
      it "reports tuple with literals" do
        s = Source.new %(
          case {1, false}
          when {1, _}
            :ok
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report tuple with non-literals" do
        s = Source.new %(
          a, b = 1, 2
          case {1, b}
          when {1, 2}
            :ok
          end
        )
        subject.catch(s).should be_valid
      end
    end

    describe "named tuple" do
      it "reports named tuple with literals" do
        s = Source.new %(
          case { name: 1, foo: :bar}
          end
        )
        subject.catch(s).should_not be_valid
      end

      it "doesn't report named tuple with non-literals" do
        s = Source.new %(
          case { name: a, foo: :bar}
          end
        )
        subject.catch(s).should be_valid
      end
    end

    it "reports rule, pos and message" do
      s = Source.new %(
        puts "hello" if true
      ), "source.cr"
      subject.catch(s).should_not be_valid

      s.issues.size.should eq 1
      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.to_s.should eq "source.cr:1:20"
      issue.message.should eq "Literal value found in conditional"
    end
  end
end
