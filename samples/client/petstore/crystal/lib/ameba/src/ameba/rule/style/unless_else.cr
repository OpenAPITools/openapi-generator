module Ameba::Rule::Style
  # A rule that disallows the use of an `else` block with the `unless`.
  #
  # For example, the rule considers these valid:
  #
  # ```
  # unless something
  #   :ok
  # end
  #
  # if something
  #   :one
  # else
  #   :two
  # end
  # ```
  #
  # But it considers this one invalid as it is an `unless` with an `else`:
  #
  # ```
  # unless something
  #   :one
  # else
  #   :two
  # end
  # ```
  #
  # The solution is to swap the order of the blocks, and change the `unless` to
  # an `if`, so the previous invalid example would become this:
  #
  # ```
  # if something
  #   :two
  # else
  #   :one
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/UnlessElse:
  #   Enabled: true
  # ```
  #
  struct UnlessElse < Base
    properties do
      description "Disallows the use of an `else` block with the `unless`"
    end

    MSG = "Favour if over unless with else"

    def test(source, node : Crystal::Unless)
      return if node.else.nop?
      issue_for node, MSG
    end
  end
end
