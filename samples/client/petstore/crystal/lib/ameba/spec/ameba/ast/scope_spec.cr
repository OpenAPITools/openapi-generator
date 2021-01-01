require "../../spec_helper"

module Ameba::AST
  describe Scope do
    describe "#initialize" do
      source = "a = 2"

      it "assigns outer scope" do
        root = Scope.new as_node(source)
        child = Scope.new as_node(source), root
        child.outer_scope.should_not be_nil
      end

      it "assigns node" do
        scope = Scope.new as_node(source)
        scope.node.should_not be_nil
      end
    end
  end

  describe "delegation" do
    it "delegates to_s to node" do
      node = as_node("def foo; end")
      scope = Scope.new node
      scope.to_s.should eq node.to_s
    end

    it "delegates locations to node" do
      node = as_node("def foo; end")
      scope = Scope.new node
      scope.location.should eq node.location
      scope.end_location.should eq node.end_location
    end
  end

  describe "#references" do
    it "can return an empty list of references" do
      scope = Scope.new as_node("")
      scope.references.should be_empty
    end

    it "allows to add variable references" do
      scope = Scope.new as_node("")
      nodes = as_nodes "a = 2"
      scope.references << Reference.new(nodes.var_nodes.first, scope)
      scope.references.size.should eq 1
    end
  end

  describe "#add_variable" do
    it "adds a new variable to the scope" do
      scope = Scope.new as_node("")
      scope.add_variable(Crystal::Var.new "foo")
      scope.variables.any?.should be_true
    end
  end

  describe "#find_variable" do
    it "returns the variable in the scope by name" do
      scope = Scope.new as_node("foo = 1")
      scope.add_variable Crystal::Var.new "foo"
      scope.find_variable("foo").should_not be_nil
    end

    it "returns nil if variable not exist in this scope" do
      scope = Scope.new as_node("foo = 1")
      scope.find_variable("bar").should be_nil
    end
  end

  describe "#assign_variable" do
    it "creates a new assignment" do
      scope = Scope.new as_node("foo = 1")
      scope.add_variable Crystal::Var.new "foo"
      scope.assign_variable("foo", Crystal::Var.new "foo")
      scope.find_variable("foo").not_nil!.assignments.size.should eq 1
    end

    it "does not create the assignment if variable is wrong" do
      scope = Scope.new as_node("foo = 1")
      scope.add_variable Crystal::Var.new "foo"
      scope.assign_variable("bar", Crystal::Var.new "bar")
      scope.find_variable("foo").not_nil!.assignments.size.should eq 0
    end
  end

  describe "#block?" do
    it "returns true if Crystal::Block" do
      nodes = as_nodes %(
        3.times {}
      )
      scope = Scope.new nodes.block_nodes.first
      scope.block?.should be_true
    end

    it "returns false otherwise" do
      scope = Scope.new as_node "a = 1"
      scope.block?.should be_false
    end
  end

  describe "#spawn_block?" do
    it "returns true if a node is a spawn block" do
      nodes = as_nodes %(
        spawn {}
      )
      scope = Scope.new nodes.block_nodes.first
      scope.spawn_block?.should be_true
    end

    it "returns false otherwise" do
      scope = Scope.new as_node "a = 1"
      scope.spawn_block?.should be_false
    end
  end

  describe "#in_macro?" do
    it "returns true if Crystal::Macro" do
      nodes = as_nodes %(
        macro included
        end
      )
      scope = Scope.new nodes.macro_nodes.first
      scope.in_macro?.should be_true
    end

    it "returns true if node is nested to Crystal::Macro" do
      nodes = as_nodes %(
        macro included
          {{@type.each do |type| a = type end}}
        end
      )
      outer_scope = Scope.new nodes.macro_nodes.first
      scope = Scope.new nodes.block_nodes.first, outer_scope
      scope.in_macro?.should be_true
    end

    it "returns false otherwise" do
      scope = Scope.new as_node "a = 1"
      scope.in_macro?.should be_false
    end
  end
end
