require "../../../spec_helper"

module Ameba::Rule::Style
  describe RedundantBegin do
    subject = RedundantBegin.new

    it "passes if there is no redundant begin blocks" do
      s = Source.new %(
        def method
          do_something
        rescue
          do_something_else
        end

        def method
          do_something
          do_something_else
        ensure
          handle_something
        end

        def method
          yield
        rescue
        end

        def method; end
        def method; a = 1; rescue; end
        def method; begin; rescue; end; end
      )
      subject.catch(s).should be_valid
    end

    it "passes if there is a correct begin block in a handler" do
      s = Source.new %q(
        def handler_and_expression
          begin
            open_file
          rescue
            close_file
          end
          do_some_stuff
        end

        def multiple_handlers
          begin
            begin1
          rescue
          end

          begin
            begin2
          rescue
          end
        rescue
          do_something_else
        end

        def assign_and_begin
          @result ||=
            begin
              do_something
              do_something_else
              returnit
            end
        rescue
        end

        def inner_handler
          s = begin
              rescue
              end
        rescue
        end

        def begin_and_expression
          begin
            a = 1
            b = 2
          end
          expr
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is a redundant begin block" do
      s = Source.new %q(
        def method(a : String) : String
          begin
            open_file
            do_some_stuff
          ensure
            close_file
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is a redundant begin block in a method without args" do
      s = Source.new %q(
        def method
          begin
            open_file
          ensure
            close_file
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is a redundant block in a method with return type" do
      s = Source.new %q(
        def method : String
          begin
            open_file
          ensure
            close_file
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is a redundant block in a method with multiple args" do
      s = Source.new %q(
        def method(a : String,
                  b : String)
          begin
            open_file
          ensure
            close_file
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is a redundant block in a method with multiple args" do
      s = Source.new %q(
        def method(a : String,
                  b : String
        )
          begin
            open_file
          ensure
            close_file
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if there is an inner redundant block" do
      s = Source.new %q(
        def method
          begin
            open_file
          ensure
            close_file
          end
        rescue
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is a redundant block with yield" do
      s = Source.new %q(
        def method
          begin
            yield
          ensure
            close_file
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "fails if there is top level redundant block in a method" do
      s = Source.new %q(
        def method
          begin
            a = 1
            b = 2
          end
        end
      )
      subject.catch(s).should_not be_valid
    end

    it "doesn't report if begin-end block in a proc literal" do
      s = Source.new %q(
        foo = ->{
          begin
            raise "Foo!"
          rescue ex
            pp ex
          end
        }
      )
      subject.catch(s).should be_valid
    end

    it "reports rule, pos and message" do
      s = Source.new %q(
        def method
          begin
            open_connection
          ensure
            close_connection
          end
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid

      issue = s.issues.first
      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:1:1"
      issue.end_location.to_s.should eq "source.cr:7:3"
      issue.message.should eq "Redundant `begin` block detected"
    end
  end
end
