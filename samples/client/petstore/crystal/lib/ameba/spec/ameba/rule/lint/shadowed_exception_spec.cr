require "../../../spec_helper"

private def check_shadowed(source, exceptions)
  s = Ameba::Source.new source
  Ameba::Rule::Lint::ShadowedException.new.catch(s).should_not be_valid
  s.issues.first.message.should contain exceptions.join(", ")
end

module Ameba::Rule::Lint
  describe ShadowedException do
    subject = ShadowedException.new

    it "passes if there isn't shadowed exception" do
      s = Source.new %(
        def method
          do_something
        rescue ArgumentError
          handle_argument_error_exception
        rescue Exception
          handle_exception
        end

        def method
        rescue Exception
          handle_exception
        end

        def method
        rescue e : ArgumentError
          handle_argument_error_exception
        rescue e : Exception
          handle_exception
        end
      )
      subject.catch(s).should be_valid
    end

    it "fails if there is a shadowed exception" do
      check_shadowed %(
        begin
          do_something
        rescue Exception
          handle_exception
        rescue ArgumentError
          handle_argument_error_exception
        end
      ), %w(ArgumentError)
    end

    it "fails if there is a custom shadowed exceptions" do
      check_shadowed %(
        begin
          1
        rescue Exception
          2
        rescue MySuperException
          3
        end
      ), %w(MySuperException)
    end

    it "fails if there is a shadowed exception in a type list" do
      check_shadowed %(
        begin
        rescue Exception | IndexError
        end
      ), %w(IndexError)
    end

    it "fails if there is a first shadowed exception in a type list" do
      check_shadowed %(
        begin
        rescue IndexError | Exception
        rescue Exception
        rescue
        end
      ), %w(IndexError)
    end

    it "fails if there is a shadowed duplicated exception" do
      check_shadowed %(
        begin
        rescue IndexError
        rescue ArgumentError
        rescue IndexError
        end
      ), %w(IndexError)
    end

    it "fails if there is a shadowed duplicated exception in a type list" do
      check_shadowed %(
        begin
        rescue IndexError
        rescue ArgumentError | IndexError
        end
      ), %w(IndexError)
    end

    it "fails if there is only shadowed duplicated exceptions" do
      check_shadowed %(
        begin
        rescue IndexError
        rescue IndexError
        end
      ), %w(IndexError)
    end

    it "fails if there is only shadowed duplicated exceptions in a type list" do
      check_shadowed %(
        begin
        rescue IndexError | IndexError
        end
      ), %w(IndexError)
    end

    it "fails if all rescues are shadowed and there is a catch-all rescue" do
      check_shadowed %(
        begin
        rescue Exception
        rescue ArgumentError
        rescue IndexError
        rescue KeyError | IO::Error
        rescue
        end
      ), %w(IndexError KeyError IO::Error)
    end

    it "fails if there are shadowed exception with args" do
      check_shadowed %(
        begin
        rescue Exception
        rescue ex : IndexError
        rescue
        end
      ), %w(IndexError)
    end

    it "fails if there are multiple shadowed exceptions" do
      check_shadowed %(
        begin
        rescue Exception
        rescue ArgumentError
        rescue IndexError
        end
      ), %w(ArgumentError IndexError)
    end

    it "fails if there are multiple shadowed exceptions in a type list" do
      check_shadowed %(
        begin
        rescue Exception
        rescue ArgumentError | IndexError
        rescue IO::Error
        end
      ), %w(ArgumentError IndexError IO::Error)
    end

    it "reports rule, location and a message" do
      s = Source.new %q(
        begin
          do_something
        rescue Exception | IndexError
        end
      ), "source.cr"
      subject.catch(s).should_not be_valid
      issue = s.issues.first

      issue.rule.should_not be_nil
      issue.location.to_s.should eq "source.cr:2:3"
      issue.end_location.to_s.should eq "source.cr:4:3"
      issue.message.should eq(
        "Exception handler has shadowed exceptions: IndexError"
      )
    end
  end
end
