require "../../spec_helper"

module Ameba::AST
  describe Branchable do
    describe "#initialize" do
      it "creates a new branchable" do
        branchable = Branchable.new as_node %(a = 2 if true)
        branchable.node.should_not be_nil
      end
    end

    describe "delegation" do
      it "delegates to_s to @node" do
        node = as_node %(a = 2 if true)
        branchable = Branchable.new node
        branchable.to_s.should eq node.to_s
      end

      it "delegates locations to @node" do
        node = as_node %(a = 2 if true)
        branchable = Branchable.new node
        branchable.location.should eq node.location
        branchable.end_location.should eq node.end_location
      end
    end

    describe "#loop?" do
      it "returns true if it is a while loop" do
        branchable = Branchable.new as_node %(while true; a = 2; end)
        branchable.loop?.should be_true
      end

      it "returns true if it is the until loop" do
        branchable = Branchable.new as_node %(until false; a = 2; end)
        branchable.loop?.should be_true
      end

      it "returns true if it is loop" do
        branchable = Branchable.new as_node %(loop {})
        branchable.loop?.should be_true
      end

      it "returns false otherwise" do
        branchable = Branchable.new as_node %(a = 2 if true)
        branchable.loop?.should be_false
      end
    end
  end
end
