module Ameba::Rule::Lint
  # A rule that disallows empty ensure statement.
  #
  # For example, this is considered invalid:
  #
  # ```
  # def some_method
  #   do_some_stuff
  # ensure
  # end
  #
  # begin
  #   do_some_stuff
  # ensure
  # end
  # ```
  #
  # And it should be written as this:
  #
  #
  # ```
  # def some_method
  #   do_some_stuff
  # ensure
  #   do_something_else
  # end
  #
  # begin
  #   do_some_stuff
  # ensure
  #   do_something_else
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/EmptyEnsure
  #   Enabled: true
  # ```
  #
  struct EmptyEnsure < Base
    properties do
      description "Disallows empty ensure statement"
    end

    MSG = "Empty `ensure` block detected"

    def test(source, node : Crystal::ExceptionHandler)
      node_ensure = node.ensure
      return if node_ensure.nil? || !node_ensure.nop?

      issue_for node, MSG
    end
  end
end
