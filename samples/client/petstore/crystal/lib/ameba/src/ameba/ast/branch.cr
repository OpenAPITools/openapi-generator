module Ameba::AST
  # Represents the branch in Crystal code.
  # Branch is a part of a branchable statement.
  # For example, the branchable if statement contains 3 branches:
  #
  # ```
  # if a = something # --> Branch A
  #   a = 1          # --> Branch B
  #   put a if out   # --> Branch C
  # else
  #   do_something a # --> Branch D
  # end
  # ```
  #
  class Branch
    # The actual branch node.
    getter node : Crystal::ASTNode

    # The parent branchable.
    getter parent : Branchable

    delegate to_s, to: @node
    delegate location, to: @node
    delegate end_location, to: @node

    def_equals_and_hash node, location

    # Creates a new branch.
    #
    # ```
    # Branch.new(if_node)
    # ```
    def initialize(@node, @parent)
    end

    # Returns true if current branch is in a loop, false - otherwise.
    # For example, this branch is in a loop:
    #
    # ```
    # while true
    #   handle_input # this branch is in a loop
    #   if wrong_input
    #     show_message # this branch is also in a loop.
    #   end
    # end
    # ```
    #
    def in_loop?
      @parent.loop?
    end

    # Constructs a new branch based on the node in scope.
    #
    # ```
    # Branch.of(assign_node, scope)
    # ```
    def self.of(node : Crystal::ASTNode, scope : Scope)
      of(node, scope.node)
    end

    # Constructs a new branch based on the node some parent scope.
    #
    # ```
    # Branch.of(assign_node, def_node)
    # ```
    def self.of(node : Crystal::ASTNode, parent_node : Crystal::ASTNode)
      BranchVisitor.new(node).tap(&.accept parent_node).branch
    end

    # :nodoc:
    private class BranchVisitor < Crystal::Visitor
      @current_branch : Crystal::ASTNode?

      property branchable : Branchable?
      property branch : Branch?

      def initialize(@node : Crystal::ASTNode)
      end

      private def on_branchable_start(node, *branches)
        on_branchable_start(node, branches)
      end

      private def on_branchable_start(node, branches : Array | Tuple)
        @branchable = Branchable.new(node, @branchable)

        branches.each do |branch_node|
          break if branch # branch found
          @current_branch = branch_node if branch_node && !branch_node.nop?
          branch_node.try &.accept(self)
        end

        false
      end

      private def on_branchable_end(node)
        @branchable = @branchable.try &.parent
      end

      def visit(node : Crystal::ASTNode)
        return false if branch

        if node.class == @node.class &&
           node.location == @node.location &&
           (branchable = @branchable) &&
           (branch = @current_branch)
          @branch = Branch.new(branch, branchable)
        end

        true
      end

      def visit(node : Crystal::If)
        on_branchable_start node, node.cond, node.then, node.else
      end

      def end_visit(node : Crystal::If)
        on_branchable_end node
      end

      def visit(node : Crystal::Unless)
        on_branchable_start node, node.cond, node.then, node.else
      end

      def end_visit(node : Crystal::Unless)
        on_branchable_end node
      end

      def visit(node : Crystal::BinaryOp)
        on_branchable_start node, node.left, node.right
      end

      def end_visit(node : Crystal::BinaryOp)
        on_branchable_end node
      end

      def visit(node : Crystal::Case)
        on_branchable_start node, [node.cond, node.whens, node.else].flatten
      end

      def end_visit(node : Crystal::Case)
        on_branchable_end node
      end

      def visit(node : Crystal::While)
        on_branchable_start node, node.cond, node.body
      end

      def end_visit(node : Crystal::While)
        on_branchable_end node
      end

      def visit(node : Crystal::Until)
        on_branchable_start node, node.cond, node.body
      end

      def end_visit(node : Crystal::Until)
        on_branchable_end node
      end

      def visit(node : Crystal::ExceptionHandler)
        on_branchable_start node, [node.body, node.rescues, node.else, node.ensure].flatten
      end

      def end_visit(node : Crystal::ExceptionHandler)
        on_branchable_end node
      end

      def visit(node : Crystal::Rescue)
        on_branchable_start node, node.body
      end

      def end_visit(node : Crystal::Rescue)
        on_branchable_end node
      end

      def visit(node : Crystal::MacroIf)
        on_branchable_start node, node.cond, node.then, node.else
      end

      def end_visit(node : Crystal::MacroIf)
        on_branchable_end node
      end

      def visit(node : Crystal::MacroFor)
        on_branchable_start node, node.exp, node.body
      end

      def end_visit(node : Crystal::MacroFor)
        on_branchable_end node
      end
    end
  end
end
