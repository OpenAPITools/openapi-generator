module Ameba::AST
  # A class that utilizes a logic to traverse AST nodes and
  # fire a source test callback if a redundant `Crystal::ControlExpression`
  # is reached.
  class RedundantControlExpressionVisitor
    # A corresponding rule that uses this visitor.
    getter rule : Rule::Base

    # A source that needs to be traversed.
    getter source : Source

    # A node to run traversal on.
    getter node : Crystal::ASTNode

    def initialize(@rule, @source, @node)
      traverse_node node
    end

    private def traverse_control_expression(node)
      @rule.test(@source, node, self)
    end

    private def traverse_node(node)
      case node
      when Crystal::ControlExpression   then traverse_control_expression node
      when Crystal::Expressions         then traverse_expressions node
      when Crystal::If, Crystal::Unless then traverse_condition node
      when Crystal::Case                then traverse_case node
      when Crystal::BinaryOp            then traverse_binary_op node
      when Crystal::ExceptionHandler    then traverse_exception_handler node
      else
        # ok
      end
    end

    private def traverse_expressions(node)
      traverse_node node.expressions.last?
    end

    private def traverse_condition(node)
      return if node.else.nil? || node.else.nop?

      traverse_node(node.then)
      traverse_node(node.else)
    end

    private def traverse_case(node)
      node.whens.each { |n| traverse_node n.body }
      traverse_node(node.else)
    end

    private def traverse_binary_op(node)
      traverse_node(node.right)
    end

    private def traverse_exception_handler(node)
      traverse_node node.body
      traverse_node node.else
      node.rescues.try &.each { |n| traverse_node n.body }
    end
  end
end
