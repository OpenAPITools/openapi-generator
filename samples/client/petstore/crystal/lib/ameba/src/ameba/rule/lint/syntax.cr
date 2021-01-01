module Ameba::Rule::Lint
  # A rule that reports invalid Crystal syntax.
  #
  # For example, this syntax is invalid:
  #
  # ```
  # def hello
  #   do_something
  # rescue Exception => e
  # end
  # ```
  #
  # And should be properly written:
  #
  # ```
  # def hello
  #   do_something
  # rescue e : Exception
  # end
  # ```
  #
  struct Syntax < Base
    properties do
      description "Reports invalid Crystal syntax"
      severity Ameba::Severity::Error
    end

    def test(source)
      source.ast
    rescue e : Crystal::SyntaxException
      issue_for({e.line_number, e.column_number}, e.message.to_s)
    end
  end
end
