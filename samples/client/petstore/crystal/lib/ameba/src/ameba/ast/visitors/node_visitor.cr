require "./base_visitor"

module Ameba::AST
  # List of nodes to be visited by Ameba's rules.
  NODES = [
    Alias,
    IsA,
    Assign,
    Call,
    Block,
    Case,
    ClassDef,
    ClassVar,
    Def,
    EnumDef,
    ExceptionHandler,
    Expressions,
    HashLiteral,
    If,
    InstanceVar,
    LibDef,
    ModuleDef,
    NilLiteral,
    StringInterpolation,
    Unless,
    Var,
    When,
    While,
    Until,
  ]

  # An AST Visitor that traverses the source and allows all nodes
  # to be inspected by rules.
  #
  # ```
  # visitor = Ameba::AST::NodeVisitor.new(rule, source)
  # ```
  #
  class NodeVisitor < BaseVisitor
    @skip : Array(Crystal::ASTNode.class)?

    def initialize(@rule, @source, skip = nil)
      @skip = skip.try &.map { |el| el.as(Crystal::ASTNode.class) }
      super @rule, @source
    end

    {% for name in NODES %}
      # A visit callback for `Crystal::{{name}}` node.
      # Returns true meaning that child nodes will be traversed as well.
      def visit(node : Crystal::{{name}})
        @rule.test @source, node
        true
      end
    {% end %}

    def visit(node)
      return true unless (skip = @skip)
      !skip.includes?(node.class)
    end
  end
end
