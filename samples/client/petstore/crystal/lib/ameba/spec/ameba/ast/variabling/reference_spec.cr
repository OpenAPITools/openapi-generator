require "../../../spec_helper"

module Ameba::AST
  describe Reference do
    it "is derived from a Variable" do
      node = Crystal::Var.new "foo"
      Reference.new(node, Scope.new as_node "foo = 1").is_a?(Variable).should be_true
    end
  end
end
