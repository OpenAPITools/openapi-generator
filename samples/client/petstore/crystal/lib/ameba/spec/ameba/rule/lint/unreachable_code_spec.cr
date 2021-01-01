require "../../../spec_helper"

module Ameba::Rule::Lint
  subject = UnreachableCode.new

  describe UnreachableCode do
    context "return" do
      it "reports if there is unreachable code after return" do
        s = Source.new %(
          def foo
            a = 1
            return false
            b = 2
          end
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":4:3"
      end

      it "doesn't report if there is return in if" do
        s = Source.new %(
          def foo
            a = 1
            return false if bar
            b = 2
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there are returns in if-then-else" do
        s = Source.new %(
          if a > 0
            return :positive
          else
            return :negative
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is no else in if" do
        s = Source.new %(
          if a > 0
            return :positive
          end
          :reachable
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report return in on-line if" do
        s = Source.new %(
          return :positive if a > 0
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if return is used in a block" do
        s = Source.new %(
          def foo
            bar = obj.try do
              if something
                a = 1
              end
              return nil
            end

            bar
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is unreachable code after if-then-else" do
        s = Source.new %(
          def foo
            if a > 0
              return :positive
            else
              return :negative
            end

            :unreachable
          end
        )
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.location.to_s.should eq ":8:3"
      end

      it "reports if there is unreachable code after if-then-else-if" do
        s = Source.new %(
          def foo
            if a > 0
              return :positive
            elsif a != 0
              return :negative
            else
              return :zero
            end

            :unreachable
          end
        )
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.location.to_s.should eq ":10:3"
      end

      it "doesn't report if there is no unreachable code after if-then-else" do
        s = Source.new %(
          def foo
            if a > 0
              return :positive
            else
              return :negative
            end
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is no unreachable in inner branch" do
        s = Source.new %(
          def foo
            if a > 0
              return :positive if a != 1
            else
              return :negative
            end

            :not_unreachable
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is no unreachable in exception handler" do
        s = Source.new %(
          def foo
            puts :bar
          rescue Exception
            raise "Error!"
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is multiple conditions with return" do
        s = Source.new %(
          if :foo
            if :bar
              return :foobar
            else
              return :foobaz
            end
          elsif :fox
            return :foofox
          end

          return :reachable
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is unreachable code after unless" do
        s = Source.new %(
          unless :foo
            return :bar
          else
            return :foo
          end

          :unreachable
        )
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.location.to_s.should eq ":7:1"
      end

      it "doesn't report if there is no unreachable code after unless" do
        s = Source.new %(
          unless :foo
            return :bar
          end

          :reachable
        )
        subject.catch(s).should be_valid
      end
    end

    context "binary op" do
      it "reports unreachable code in a binary operator" do
        s = Source.new %(
          (return 22) && puts "a"
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":1:16"
      end

      it "reports unreachable code in inner binary operator" do
        s = Source.new %(
          do_something || (return 22) && puts "a"
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":1:32"
      end

      it "reports unreachable code after the binary op" do
        s = Source.new %(
          (return 22) && break
          :unreachable
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":2:1"
      end

      it "doesn't report if return is not the right" do
        s = Source.new %(
          puts "a" && return
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report unreachable code in multiple binary expressions" do
        s = Source.new %(
          foo || bar || baz
        )
        subject.catch(s).should be_valid
      end
    end

    context "case" do
      it "reports if there is unreachable code after case" do
        s = Source.new %(
          def foo
            case cond
            when 1
              something
              return
            when 2
              something2
              return
            else
              something3
              return
            end
            :unreachable
          end
        )
        subject.catch(s).should_not be_valid
        issue = s.issues.first
        issue.location.to_s.should eq ":13:3"
      end

      it "doesn't report if case does not have else" do
        s = Source.new %(
          def foo
            case cond
            when 1
              something
              return
            when 2
              something2
              return
            end
            :reachable
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if one when does not return" do
        s = Source.new %(
          def foo
            case cond
            when 1
              something
              return
            when 2
              something2
            else
              something3
              return
            end
            :reachable
          end
        )
        subject.catch(s).should be_valid
      end
    end

    context "exception handler" do
      it "reports unreachable code if it returns in body and rescues" do
        s = Source.new %(
          def foo
            begin
              return false
            rescue Error
              return false
            rescue Exception
              return false
            end
            :unreachable
          end
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":9:3"
      end

      it "reports unreachable code if it returns in rescues and else" do
        s = Source.new %(
          def foo
            begin
              do_something
            rescue Error
              return :error
            else
              return true
            end
            :unreachable
          end
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":9:3"
      end

      it "doesn't report if there is no else and ensure doesn't return" do
        s = Source.new %(
          def foo
            begin
              return false
            rescue Error
              puts "error"
            rescue Exception
              return false
            end
            :reachable
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is no else and body doesn't return" do
        s = Source.new %(
          def foo
            begin
              do_something
            rescue Error
              return true
            rescue Exception
              return false
            end
            :reachable
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is else and ensure doesn't return" do
        s = Source.new %(
          def foo
            begin
              do_something
            rescue Error
              puts "yo"
            else
              return true
            end
            :reachable
          end
        )
        subject.catch(s).should be_valid
      end

      it "doesn't report if there is else and it doesn't return" do
        s = Source.new %(
          def foo
            begin
              do_something
            rescue Error
              return false
            else
              puts "yo"
            end
            :reachable
          end
        )
        subject.catch(s).should be_valid
      end

      it "reports if there is unreachable code in rescue" do
        s = Source.new %(
          def method
          rescue
            return 22
            :unreachable
          end
        )

        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":4:3"
      end
    end

    context "while/until" do
      it "reports if there is unreachable code after while" do
        s = Source.new %(
          def method
            while something
              if :foo
                return :foo
              else
                return :foobar
              end
            end
            :unreachable
          end
        )

        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":9:3"
      end

      it "reports if there is unreachable code after until" do
        s = Source.new %(
          def method
            until something
              if :foo
                return :foo
              else
                return :foobar
              end
            end
            :unreachable
          end
        )

        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":9:3"
      end

      it "doesn't report if there is reachable code after while with break" do
        s = Source.new %(
          while something
            break
          end
          :reachable
        )

        subject.catch(s).should be_valid
      end
    end

    context "rescue" do
      it "reports unreachable code in rescue" do
        s = Source.new %(
          begin

          rescue e
            raise e
            :unreachable
          end
        )

        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":5:3"
      end

      it "doesn't report if there is no unreachable code in rescue" do
        s = Source.new %(
          begin

          rescue e
            raise e
          end
        )

        subject.catch(s).should be_valid
      end
    end

    context "when" do
      it "reports unreachable code in when" do
        s = Source.new %(
          case
          when valid?
            return 22
            :unreachable
          else

          end
        )

        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":4:3"
      end

      it "doesn't report if there is no unreachable code in when" do
        s = Source.new %(
          case
          when valid?
            return 22
          else
          end
        )

        subject.catch(s).should be_valid
      end
    end

    context "break" do
      it "reports if there is unreachable code after break" do
        s = Source.new %(
          def foo
            loop do
              break
              a = 1
            end
          end
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":4:5"
      end

      it "doesn't report if break is in a condition" do
        s = Source.new %(
          a = -100
          while true
            break if a > 0
            a += 1
          end
        )
        subject.catch(s).should be_valid
      end
    end

    context "next" do
      it "reports if there is unreachable code after next" do
        s = Source.new %(
          a = 1
          while a < 5
            next
            puts a
          end
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":4:3"
      end

      it "doesn't report if next is in a condition" do
        s = Source.new %(
          a = 1
          while a < 5
            if a == 3
              next
            end
            puts a
          end
        )
        subject.catch(s).should be_valid
      end
    end

    context "raise" do
      it "reports if there is unreachable code after raise" do
        s = Source.new %(
          a = 1
          raise "exception"
          b = 2
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":3:1"
      end

      it "doesn't report if raise is in a condition" do
        s = Source.new %(
          a = 1
          raise "exception" if a > 0
          b = 2
        )
        subject.catch(s).should be_valid
      end
    end

    context "exit" do
      it "reports if there is unreachable code after exit without args" do
        s = Source.new %(
          a = 1
          exit
          b = 2
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":3:1"
      end

      it "reports if there is unreachable code after exit with exit code" do
        s = Source.new %(
          a = 1
          exit 1
          b = 2
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":3:1"
      end

      it "doesn't report if exit is in a condition" do
        s = Source.new %(
          a = 1
          exit if a > 0
          b = 2
        )
        subject.catch(s).should be_valid
      end
    end

    context "abort" do
      it "reports if there is unreachable code after abort with one argument" do
        s = Source.new %(
          a = 1
          abort "abort"
          b = 2
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":3:1"
      end

      it "reports if there is unreachable code after abort with two args" do
        s = Source.new %(
          a = 1
          abort "abort", 1
          b = 2
        )
        subject.catch(s).should_not be_valid

        issue = s.issues.first
        issue.location.to_s.should eq ":3:1"
      end

      it "doesn't report if abort is in a condition" do
        s = Source.new %(
          a = 1
          abort "abort" if a > 0
          b = 2
        )
        subject.catch(s).should be_valid
      end
    end

    it "reports message, rule, location" do
      s = Source.new %(
        return
        :unreachable
      ), "source.cr"

      subject.catch(s).should_not be_valid

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:1"
      issue.end_location.to_s.should eq "source.cr:2:12"
      issue.message.should eq "Unreachable code detected"
    end
  end
end
