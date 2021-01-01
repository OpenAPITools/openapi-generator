module Ameba::Rule::Lint
  # A rule that disallows empty expressions.
  #
  # This is considered invalid:
  #
  # ```
  # foo = ()
  #
  # if ()
  #   bar
  # end
  # ```
  #
  # And this is valid:
  #
  # ```
  # foo = (some_expression)
  #
  # if (some_expression)
  #   bar
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/EmptyExpression:
  #   Enabled: true
  # ```
  #
  struct EmptyExpression < Base
    include AST::Util

    properties do
      description "Disallows empty expressions"
      enabled false
    end

    MSG      = "Avoid empty expression %s"
    MSG_EXRS = "Avoid empty expressions"

    def test(source, node : Crystal::NilLiteral)
      exp = node_source(node, source.lines).try &.join

      return if exp.nil? || exp == "nil"

      issue_for node, MSG % exp
    end

    def test(source, node : Crystal::Expressions)
      if node.expressions.size == 1 && node.expressions.first.nop?
        issue_for node, MSG_EXRS
      end
    end
  end
end
