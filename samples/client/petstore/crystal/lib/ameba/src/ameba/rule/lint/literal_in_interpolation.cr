module Ameba::Rule::Lint
  # A rule that disallows useless string interpolations
  # that contain a literal value instead of a variable or function.
  #
  # For example:
  #
  # ```
  # "Hello, #{:Ary}"
  # "There are #{4} cats"
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/LiteralInInterpolation
  #   Enabled: true
  # ```
  #
  struct LiteralInInterpolation < Base
    include AST::Util

    properties do
      description "Disallows useless string interpolations"
    end

    MSG = "Literal value found in interpolation"

    def test(source, node : Crystal::StringInterpolation)
      node.expressions
        .select { |e| !e.is_a?(Crystal::StringLiteral) && literal?(e) }
        .each { |n| issue_for n, MSG }
    end
  end
end
