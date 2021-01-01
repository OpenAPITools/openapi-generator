module Ameba::Rule::Lint
  # A rule that reports unreachable code.
  #
  # For example, this is considered invalid:
  #
  # ```
  # def method(a)
  #   return 42
  #   a + 1
  # end
  # ```
  #
  # ```
  # a = 1
  # loop do
  #   break
  #   a += 1
  # end
  # ```
  #
  # And has to be written as the following:
  #
  # ```
  # def method(a)
  #   return 42 if a == 0
  #   a + 1
  # end
  # ```
  #
  # ```
  # a = 1
  # loop do
  #   break a > 3
  #   a += 1
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/UnreachableCode:
  #   Enabled: true
  # ```
  #
  struct UnreachableCode < Base
    include AST::Util

    properties do
      description "Reports unreachable code"
    end

    MSG = "Unreachable code detected"

    def test(source)
      AST::FlowExpressionVisitor.new self, source
    end

    def test(source, node, flow_expression : AST::FlowExpression)
      if unreachable_node = flow_expression.unreachable_nodes.first?
        issue_for unreachable_node, MSG
      end
    end
  end
end
