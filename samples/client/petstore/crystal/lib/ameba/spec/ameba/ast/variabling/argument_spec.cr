require "../../../spec_helper"

module Ameba::AST
  describe Argument do
    arg = Crystal::Arg.new "a"
    scope = Scope.new as_node "foo = 1"
    variable = Variable.new(Crystal::Var.new("foo"), scope)

    describe "#initialize" do
      it "creates a new argument" do
        argument = Argument.new(arg, variable)
        argument.node.should_not be_nil
      end
    end

    describe "delegation" do
      it "delegates locations to node" do
        argument = Argument.new(arg, variable)
        argument.location.should eq arg.location
        argument.end_location.should eq arg.end_location
      end

      it "delegates to_s to node" do
        argument = Argument.new(arg, variable)
        argument.to_s.should eq arg.to_s
      end
    end

    describe "#ignored?" do
      it "is true if arg starts with _" do
        argument = Argument.new(Crystal::Arg.new("_a"), variable)
        argument.ignored?.should be_true
      end

      it "is false otherwise" do
        argument = Argument.new(arg, variable)
        argument.ignored?.should be_false
      end
    end
  end
end
