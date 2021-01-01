module Ameba::Rule::Lint
  # A rule that disallows string conversion in string interpolation,
  # which is redundant.
  #
  # For example, this is considered invalid:
  #
  # ```
  # "Hello, #{name.to_s}"
  # ```
  #
  # And this is valid:
  #
  # ```
  # "Hello, #{name}"
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/RedundantStringCoersion
  #   Enabled: true
  # ```
  #
  struct RedundantStringCoercion < Base
    include AST::Util

    properties do
      description "Disallows redundant string conversions in interpolation"
    end

    MSG = "Redundant use of `Object#to_s` in interpolation"

    def test(source, node : Crystal::StringInterpolation)
      string_coercion_nodes(node).each { |n| issue_for n.name_location, n.end_location, MSG }
    end

    private def string_coercion_nodes(node)
      node.expressions.select do |e|
        e.is_a?(Crystal::Call) &&
          e.name == "to_s" &&
          e.args.size.zero? &&
          e.named_args.nil? &&
          e.obj
      end
    end
  end
end
