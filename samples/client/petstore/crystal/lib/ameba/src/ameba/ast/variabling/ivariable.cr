module Ameba::AST
  class InstanceVariable
    getter node : Crystal::InstanceVar

    delegate location, to: @node
    delegate end_location, to: @node
    delegate name, to: @node
    delegate to_s, to: @node

    def initialize(@node)
    end
  end
end
