module Ameba::AST
  # Represents the existence of the local variable.
  # Holds the var node and variable assigments.
  class Variable
    # List of the assigments of this variable.
    getter assignments = [] of Assignment

    # List of the references of this variable.
    getter references = [] of Reference

    # The actual var node.
    getter node : Crystal::Var

    # Scope of this variable.
    getter scope : Scope

    # Node of the first assignment which can be available before any reference.
    getter assign_before_reference : Crystal::ASTNode?

    delegate location, to: @node
    delegate end_location, to: @node
    delegate name, to: @node
    delegate to_s, to: @node

    # Creates a new variable(in the scope).
    #
    # ```
    # Variable.new(node, scope)
    # ```
    #
    def initialize(@node, @scope)
    end

    # Returns true if it is a special variable, i.e `$?`.
    def special?
      @node.special_var?
    end

    # Assigns the variable (creates a new assignment).
    # Variable may have multiple assignments.
    #
    # ```
    # variable = Variable.new(node, scope)
    # variable.assign(node1)
    # variable.assign(node2)
    # variable.assignment.size # => 2
    # ```
    #
    def assign(node, scope)
      assignments << Assignment.new(node, self, scope)

      update_assign_reference!
    end

    # Returns true if variable has any reference.
    #
    # ```
    # variable = Variable.new(node, scope)
    # variable.reference(var_node)
    # variable.referenced? # => true
    # ```
    def referenced?
      references.any?
    end

    # Creates a reference to this variable in some scope.
    #
    # ```
    # variable = Variable.new(node, scope)
    # variable.reference(var_node, some_scope)
    # ```
    #
    def reference(node : Crystal::Var, scope : Scope)
      Reference.new(node, scope).tap do |reference|
        references << reference
        scope.references << reference
      end
    end

    # Reference variable's assignments.
    #
    # ```
    # variable = Variable.new(node, scope)
    # variable.assign(assign_node)
    # variable.reference_assignments!
    # ```
    def reference_assignments!
      consumed_branches = Set(Branch).new

      assignments.reverse_each do |assignment|
        next if consumed_branches.includes?(assignment.branch)
        assignment.referenced = true

        break unless assignment.branch
        consumed_branches << assignment.branch.not_nil!
      end
    end

    # Returns true if the current var is referenced in
    # in the block. For example this variable is captured
    # by block:
    #
    # ```
    # a = 1
    # 3.times { |i| a = a + i }
    # ```
    #
    # And this variable is not captured by block.
    #
    # ```
    # i = 1
    # 3.times { |i| i + 1 }
    # ```
    def captured_by_block?(scope = @scope)
      scope.inner_scopes.each do |inner_scope|
        return true if inner_scope.block? && inner_scope.references?(self)
        return true if captured_by_block?(inner_scope)
      end

      false
    end

    # Returns true if current variable potentially referenced in a macro literal,
    # false if not.
    def used_in_macro?(scope = @scope)
      scope.inner_scopes.each do |inner_scope|
        return true if MacroLiteralFinder.new(inner_scope.node).references? node
      end
      return true if (outer_scope = scope.outer_scope) && used_in_macro?(outer_scope)
      false
    end

    # Returns true if the variable is a target (on the left) of the assignment,
    # false otherwise.
    def target_of?(assign)
      case assign
      when Crystal::Assign           then eql?(assign.target)
      when Crystal::OpAssign         then eql?(assign.target)
      when Crystal::MultiAssign      then assign.targets.any? { |t| eql?(t) }
      when Crystal::UninitializedVar then eql?(assign.var)
      else
        false
      end
    end

    # Returns true if the name starts with '_', false if not.
    def ignored?
      name.starts_with? '_'
    end

    # Returns true if the `node` represents exactly
    # the same Crystal node as `@node`.
    def eql?(node)
      node.is_a?(Crystal::Var) &&
        node.name == @node.name &&
        node.location == @node.location
    end

    # Returns true if the variable is delcared before the `node`.
    def declared_before?(node)
      var_location, node_location = location, node.location

      return if var_location.nil? || node_location.nil?

      (var_location.line_number < node_location.line_number) ||
        (var_location.line_number == node_location.line_number &&
          var_location.column_number < node_location.column_number)
    end

    private class MacroLiteralFinder < Crystal::Visitor
      @macro_literals = [] of Crystal::MacroLiteral

      def initialize(node)
        node.accept self
      end

      def references?(node : Crystal::Var)
        @macro_literals.any? { |literal| literal.value.includes? node.name }
      end

      def visit(node : Crystal::ASTNode)
        true
      end

      def visit(node : Crystal::MacroLiteral)
        @macro_literals << node
      end
    end

    private def update_assign_reference!
      if @assign_before_reference.nil? &&
         references.size <= assignments.size &&
         assignments.none? { |ass| ass.op_assign? }
        @assign_before_reference = assignments.find { |ass| !ass.in_branch? }.try &.node
      end
    end
  end
end
