require "./util"

module Ameba::AST
  # Represents a flow expression in Crystal code.
  # For example,
  #
  # ```
  # def foobar
  #   a = 3
  #   return 42 # => flow expression
  #   a + 1
  # end
  # ```
  #
  # Flow expression contains an actual node of a control expression and
  # a parent node, which allows easily search through the related statement
  # (i.e. find unreachable code)
  class FlowExpression
    include Util

    # Is true only if some of the nodes parents is a loop.
    getter? in_loop : Bool

    # The actual node of the flow expression.
    getter node : Crystal::ASTNode

    delegate to_s, to: @node
    delegate location, to: @node
    delegate end_location, to: @node

    # Creates a new flow expression.
    #
    # ```
    # FlowExpression.new(node, parent_node)
    # ```
    def initialize(@node, @in_loop)
    end

    # Returns nodes which can't be reached because of a flow command inside.
    # For example:
    #
    # ```
    # def foobar
    #   a = 1
    #   return 42
    #
    #   a + 2 # => unreachable assign node
    # end
    # ```
    def unreachable_nodes
      unreachable_nodes = [] of Crystal::ASTNode

      case current_node = node
      when Crystal::Expressions
        control_flow_found = false
        current_node.expressions.each do |exp|
          unreachable_nodes << exp if control_flow_found
          control_flow_found ||= flow_expression?(exp, in_loop?)
        end
      when Crystal::BinaryOp
        unreachable_nodes << current_node.right if flow_expression?(current_node.left, in_loop?)
      else
        # nop
      end

      unreachable_nodes
    end
  end
end
