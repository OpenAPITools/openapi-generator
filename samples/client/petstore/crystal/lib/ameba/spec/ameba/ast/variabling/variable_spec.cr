require "../../../spec_helper"

module Ameba::AST
  describe Variable do
    var_node = Crystal::Var.new("foo")
    scope = Scope.new as_node "foo = 1"

    describe "#initialize" do
      it "creates a new variable" do
        variable = Variable.new(var_node, scope)
        variable.node.should_not be_nil
      end
    end

    describe "delegation" do
      it "delegates locations" do
        variable = Variable.new(var_node, scope)
        variable.location.should eq var_node.location
        variable.end_location.should eq var_node.end_location
      end

      it "delegates name" do
        variable = Variable.new(var_node, scope)
        variable.name.should eq var_node.name
      end

      it "delegates to_s" do
        variable = Variable.new(var_node, scope)
        variable.to_s.should eq var_node.to_s
      end
    end

    describe "#special?" do
      it "returns truthy if it is a special `$?` var" do
        variable = Variable.new Crystal::Var.new("$?"), scope
        variable.special?.should be_truthy
      end

      it "returns falsey otherwise" do
        variable = Variable.new Crystal::Var.new("a"), scope
        variable.special?.should be_falsey
      end
    end

    describe "#assign" do
      assign_node = as_node("foo=1")

      it "assigns the variable (creates a new assignment)" do
        variable = Variable.new(var_node, scope)
        variable.assign(assign_node, scope)
        variable.assignments.any?.should be_true
      end

      it "can create multiple assignments" do
        variable = Variable.new(var_node, scope)
        variable.assign(assign_node, scope)
        variable.assign(assign_node, scope)
        variable.assignments.size.should eq 2
      end
    end

    describe "#reference" do
      it "references the existed assignment" do
        variable = Variable.new(var_node, scope)
        variable.assign(as_node("foo=1"), scope)
        variable.reference(var_node, scope)
        variable.references.any?.should be_true
      end

      it "adds a reference to the scope" do
        scope = Scope.new as_node "foo = 1"
        variable = Variable.new(var_node, scope)
        variable.assign(as_node("foo=1"), scope)
        variable.reference(var_node, scope)
        scope.references.size.should eq 1
        scope.references.first.node.to_s.should eq "foo"
      end
    end

    describe "#captured_by_block?" do
      it "returns truthy if the variable is captured by block" do
        nodes = as_nodes %(
          def method
            a = 2
            3.times { |i| a = a + i }
          end
        )
        scope = Scope.new nodes.def_nodes.first
        var_node = nodes.var_nodes.first
        scope.add_variable var_node
        scope.inner_scopes << Scope.new(nodes.block_nodes.first, scope)

        variable = Variable.new(var_node, scope)
        variable.reference nodes.var_nodes.last, scope.inner_scopes.last
        variable.captured_by_block?.should be_truthy
      end

      it "returns falsey if the variable is not captured by the block" do
        scope = Scope.new as_node %(
          def method
            a = 1
          end
        )
        scope.add_variable Crystal::Var.new "a"
        variable = scope.variables.first
        variable.captured_by_block?.should be_falsey
      end
    end

    describe "#target_of?" do
      it "returns true if the variable is a target of Crystal::Assign node" do
        assign_node = as_nodes("foo=1").assign_nodes.last
        variable = Variable.new assign_node.target.as(Crystal::Var), scope
        variable.target_of?(assign_node).should be_true
      end

      it "returns true if the variable is a target of Crystal::OpAssign node" do
        assign_node = as_nodes("foo=1;foo+=1").op_assign_nodes.last
        variable = Variable.new assign_node.target.as(Crystal::Var), scope
        variable.target_of?(assign_node).should be_true
      end

      it "returns true if the variable is a target of Crystal::MultiAssign node" do
        assign_node = as_nodes("a,b,c={1,2,3}").multi_assign_nodes.last
        assign_node.targets.size.should_not eq 0
        assign_node.targets.each do |target|
          variable = Variable.new target.as(Crystal::Var), scope
          variable.target_of?(assign_node).should be_true
        end
      end

      it "returns false if the node is not assign" do
        variable = Variable.new(Crystal::Var.new("v"), scope)
        variable.target_of?(as_node "nil").should be_false
      end

      it "returns false if the variable is not a target of the assign" do
        variable = Variable.new(Crystal::Var.new("foo"), scope)
        variable.target_of?(as_node("bar = 1")).should be_false
      end
    end

    describe "#ignored?" do
      it "is true if variable is ignored" do
        variable = Variable.new(Crystal::Var.new("_var"), scope)
        variable.ignored?.should be_true
      end

      it "is false if variable is not ignored" do
        variable = Variable.new(Crystal::Var.new("v_ar"), scope)
        variable.ignored?.should be_false
      end

      it "is true if variable is a black hole" do
        variable = Variable.new(Crystal::Var.new("_"), scope)
        variable.ignored?.should be_true
      end
    end

    describe "#eql?" do
      var = Crystal::Var.new("foo").at(Crystal::Location.new(nil, 1, 2))
      variable = Variable.new var, scope

      it "is false if node is not a Crystal::Var" do
        variable.eql?(as_node("nil")).should be_false
      end

      it "is false if node name is different" do
        variable.eql?(Crystal::Var.new "bar").should be_false
      end

      it "is false if node has a different location" do
        variable.eql?(Crystal::Var.new "foo").should be_false
      end

      it "is true otherwise" do
        variable.eql?(variable.node).should be_true
      end
    end

    describe "#declared_before?" do
      it "is falsey if variable doesn't have location" do
        var1 = Crystal::Var.new("foo")
        var2 = Crystal::Var.new("bar").at(Crystal::Location.new(nil, 1, 2))
        Variable.new(var1, scope).declared_before?(var2).should be_falsey
      end

      it "is falsey if node doesn't have location" do
        var1 = Crystal::Var.new("foo").at(Crystal::Location.new(nil, 1, 2))
        var2 = Crystal::Var.new("bar")
        Variable.new(var1, scope).declared_before?(var2).should be_falsey
      end

      it "is true if var's line_number below the node" do
        var1 = Crystal::Var.new("foo").at(Crystal::Location.new(nil, 1, 2))
        var2 = Crystal::Var.new("bar").at(Crystal::Location.new(nil, 2, 2))
        Variable.new(var1, scope).declared_before?(var2).should be_true
      end

      it "is true if var's column_number is after the node" do
        var1 = Crystal::Var.new("foo").at(Crystal::Location.new(nil, 1, 2))
        var2 = Crystal::Var.new("bar").at(Crystal::Location.new(nil, 1, 3))
        Variable.new(var1, scope).declared_before?(var2).should be_true
      end

      it "is false if var's location is before the node" do
        var1 = Crystal::Var.new("foo").at(Crystal::Location.new(nil, 2, 2))
        var2 = Crystal::Var.new("bar").at(Crystal::Location.new(nil, 1, 3))
        Variable.new(var1, scope).declared_before?(var2).should be_false
      end

      it "is false is the node is the same var" do
        var = Crystal::Var.new("foo").at(Crystal::Location.new(nil, 2, 2))
        Variable.new(var, scope).declared_before?(var).should be_false
      end
    end
  end
end
