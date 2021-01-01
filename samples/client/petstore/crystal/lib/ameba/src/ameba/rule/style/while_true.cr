module Ameba::Rule::Style
  # A rule that disallows the use of `while true` instead of using the idiomatic `loop`
  #
  # For example, this is considered invalid:
  #
  # ```
  # while true
  #   do_something
  #   break if some_condition
  # end
  # ```
  #
  # And should be replaced by the following:
  #
  # ```
  # loop do
  #   do_something
  #   break if some_condition
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/WhileTrue:
  #   Enabled: true
  # ```
  #
  struct WhileTrue < Base
    properties do
      description "Disallows while statements with a true literal as condition"
    end

    MSG = "While statement using true literal as condition"

    def test(source, node : Crystal::While)
      return unless node.cond.true_literal?
      issue_for node, MSG
    end
  end
end
