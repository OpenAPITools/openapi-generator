module Ameba::AST
  # Represents the argument of some node.
  # Holds the reference to the variable, thus to scope.
  #
  # For example, all these vars are arguments:
  #
  # ```
  # def method(a, b, c = 10, &block)
  #   3.times do |i|
  #   end
  #
  #   ->(x : Int32) {}
  # end
  # ```
  class Argument
    # The actual node.
    getter node : Crystal::Var | Crystal::Arg

    # Variable of this argument (may be the same node)
    getter variable : Variable

    delegate location, to: @node
    delegate end_location, to: @node
    delegate to_s, to: @node

    # Creates a new argument.
    #
    # ```
    # Argument.new(node, variable)
    # ```
    def initialize(@node, @variable)
    end

    # Returns true if the name starts with '_', false if not.
    def ignored?
      name.starts_with? '_'
    end

    # Name of the argument.
    def name
      case current_node = node
      when Crystal::Var then current_node.name
      when Crystal::Arg then current_node.name
      else
        raise ArgumentError.new "invalid node"
      end
    end
  end
end
