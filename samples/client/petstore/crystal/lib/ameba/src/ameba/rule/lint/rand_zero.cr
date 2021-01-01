module Ameba::Rule::Lint
  # A rule that disallows `rand(0)` and `rand(1)` calls.
  # Such calls always return `0`.
  #
  # For example:
  #
  # ```
  # rand(1)
  # ```
  #
  # Should be written as:
  #
  # ```
  # rand
  # # or
  # rand(2)
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/RandZero:
  #   Enabled: true
  # ```
  #
  struct RandZero < Base
    properties do
      description "Disallows rand zero calls"
    end

    MSG = "%s always returns 0"

    def test(source, node : Crystal::Call)
      return unless node.name == "rand" &&
                    node.args.size == 1 &&
                    (arg = node.args.first) &&
                    (arg.is_a? Crystal::NumberLiteral) &&
                    (value = arg.value) &&
                    (value == "0" || value == "1")

      issue_for node, MSG % node
    end
  end
end
