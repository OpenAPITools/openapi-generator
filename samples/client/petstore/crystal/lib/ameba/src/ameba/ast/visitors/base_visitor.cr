require "compiler/crystal/syntax/*"

# A module that helps to traverse Crystal AST using `Crystal::Visitor`.
module Ameba::AST
  # An abstract base visitor that utilizes general logic for all visitors.
  abstract class BaseVisitor < Crystal::Visitor
    # A corresponding rule that uses this visitor.
    @rule : Rule::Base

    # A source that needs to be traversed.
    @source : Source

    # Creates instance of this visitor.
    #
    # ```
    # visitor = Ameba::AST::NodeVisitor.new(rule, source)
    # ```
    #
    def initialize(@rule, @source)
      @source.ast.accept self
    end

    # A main visit method that accepts `Crystal::ASTNode`.
    # Returns true meaning all child nodes will be traversed.
    def visit(node : Crystal::ASTNode)
      true
    end
  end
end
