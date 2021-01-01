require "../util"
require "./base_visitor"

module Ameba::AST
  # AST Visitor that traverses all the flow expressions.
  class FlowExpressionVisitor < BaseVisitor
    include Util

    @loop_stack = [] of Crystal::ASTNode

    # Creates a new flow expression visitor.
    def initialize(@rule, @source)
      @source.ast.accept self
    end

    # :nodoc:
    def visit(node)
      if flow_expression?(node, in_loop?)
        @rule.test @source, node, FlowExpression.new(node, in_loop?)
      end

      true
    end

    # :nodoc:
    def visit(node : Crystal::While)
      on_loop_started(node)
    end

    # :nodoc:
    def visit(node : Crystal::Until)
      on_loop_started(node)
    end

    # :nodoc:
    def visit(node : Crystal::Call)
      on_loop_started(node) if loop?(node)
    end

    # :nodoc:
    def end_visit(node : Crystal::While)
      on_loop_ended(node)
    end

    # :nodoc:
    def end_visit(node : Crystal::Until)
      on_loop_ended(node)
    end

    # :nodoc:
    def end_visit(node : Crystal::Call)
      on_loop_ended(node) if loop?(node)
    end

    private def on_loop_started(node)
      @loop_stack.push(node)
    end

    private def on_loop_ended(node)
      @loop_stack.pop?
    end

    private def in_loop?
      @loop_stack.any?
    end
  end
end
