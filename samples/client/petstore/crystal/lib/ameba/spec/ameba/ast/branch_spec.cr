require "../../spec_helper"

private def branch_of_assign_in_def(source)
  nodes = as_nodes source
  Ameba::AST::Branch.of(nodes.assign_nodes.first, nodes.def_nodes.first)
end

module Ameba::AST
  describe Branch do
    describe ".of" do
      context "Crystal::If" do
        it "constructs a branch in If.cond" do
          branch = branch_of_assign_in_def %(
            def method
              if a = get_something # --> Crystal::Assign
                puts a
              end
            end
          )
          branch.to_s.should eq "a = get_something"
        end

        it "constructs a branch in If.then" do
          branch = branch_of_assign_in_def %(
            def method
              if true
                a = 2 # --> Crystal::Assign
              end
            end
          )
          branch.to_s.should eq "a = 2"
        end

        it "constructs a branch in If.else" do
          branch = branch_of_assign_in_def %(
            def method
              if true
                nil
              else
                a = 2 # --> Crystal::Assign
              end
            end
          )
          branch.to_s.should eq "a = 2"
        end

        it "constructs a branch in inline If" do
          branch = branch_of_assign_in_def %(
            def method(a)
              a = 0 if a == 2 # --> Crystal::Assign
            end
          )
          branch.to_s.should eq "a = 0"
        end
      end

      context "Crystal::Unless" do
        it "constructs a branch in Unless.cond" do
          branch = branch_of_assign_in_def %(
            def method
              unless a = get_something # --> Crystal::Assign
                puts a
              end
            end
          )
          branch.to_s.should eq "a = get_something"
        end

        it "constructs a branch in Unless.then" do
          branch = branch_of_assign_in_def %(
            def method
              unless true
                a = 2 # --> Crystal::Assign
              end
            end
          )
          branch.to_s.should eq "a = 2"
        end

        it "constructs a new branch in Unless.else" do
          branch = branch_of_assign_in_def %(
            def method
              unless true
                nil
              else
                a = 2 # --> Crystal::Assign
              end
            end
          )
          branch.to_s.should eq "a = 2"
        end

        it "constructs a branch in inline Unless" do
          branch = branch_of_assign_in_def %(
            def method(a)
              (a = 0; b = 3) unless a == 2 # --> Crystal::Expressions
            end
          )
          branch.to_s.should eq "(a = 0\nb = 3)"
        end
      end

      context "Crystal::BinaryOp" do
        it "constructs a branch in left node" do
          branch = branch_of_assign_in_def %(
            def method(a)
              (a = 2) && do_something
            end
          )
          branch.to_s.should eq "(a = 2)"
        end

        it "constructs a branch in right node" do
          branch = branch_of_assign_in_def %(
            def method(a)
              do_something || (a = 0)
            end
          )
          branch.to_s.should eq "(a = 0)"
        end
      end

      context "Crystal::Case" do
        it "constructs a branch in cond" do
          branch = branch_of_assign_in_def %(
            def method(a)
              case (a = 2)
              when true then nil
              end
            end
          )
          branch.to_s.should eq "(a = 2)"
        end

        it "constructs a branch in when" do
          branch = branch_of_assign_in_def %(
            def method(a)
              case a
              when a = 3 then nil
              end
            end
          )
          branch.to_s.should eq "when a = 3\n  nil\n"
        end

        it "constructs a branch in else" do
          branch = branch_of_assign_in_def %(
            def method(a)
              case a
              when true then nil
              else a = 4
              end
            end
          )
          branch.to_s.should eq "a = 4"
        end
      end

      context "Crystal::While" do
        it "constructs a branch in cond" do
          branch = branch_of_assign_in_def %(
            def method(a)
              while a = 1
                nil
              end
            end
          )
          branch.to_s.should eq "a = 1"
        end

        it "constructs a branch in body" do
          branch = branch_of_assign_in_def %(
            def method(a)
              while true
                b = (a = 1)
              end
            end
          )
          branch.to_s.should eq "b = (a = 1)"
        end
      end

      context "Crystal::Until" do
        it "constructs a branch in cond" do
          branch = branch_of_assign_in_def %(
            def method(a)
              until a = 1
                nil
              end
            end
          )
          branch.to_s.should eq "a = 1"
        end

        it "constructs a branch in body" do
          branch = branch_of_assign_in_def %(
            def method(a)
              until false
                b = (a = 1)
              end
            end
          )
          branch.to_s.should eq "b = (a = 1)"
        end
      end

      context "Crystal::ExceptionHandler" do
        it "constructs a branch in body" do
          branch = branch_of_assign_in_def %(
            def method(a)
              a = 1
            rescue
              nil
            end
          )
          branch.to_s.should eq "a = 1"
        end

        it "constructs a branch in a rescue" do
          branch = branch_of_assign_in_def %(
            def method(a)
            rescue
              a = 1
            end
          )
          branch.to_s.should eq "a = 1"
        end

        it "constructs a branch in else" do
          branch = branch_of_assign_in_def %(
            def method(a)
            rescue
            else
              a = 1
            end
          )
          branch.to_s.should eq "a = 1"
        end

        it "constructs a branch in ensure" do
          branch = branch_of_assign_in_def %(
            def method(a)
            rescue
            ensure
              a = 1
            end
          )
          branch.to_s.should eq "a = 1"
        end
      end

      context "Crystal::MacroIf" do
        it "constructs a branch in cond" do
          branch = branch_of_assign_in_def %(
            def method(a)
              {% if a = 2 %}
              {% end %}
            end
          )
          branch.to_s.should eq "a = 2"
        end

        it "constructs a branch in then" do
          nodes = as_nodes %(
            def method(a)
              {% if true %}
                a = 2
              {% end %}
            end
          )
          branch = Branch.of(nodes.macro_literal_nodes.first, nodes.def_nodes.first)
          branch.to_s.strip.should eq "a = 2"
        end
      end

      context "Crystal::MacroFor" do
        it "constructs a branch in body" do
          nodes = as_nodes %(
            def method(a)
              {% for x in [1, 2, 3] %}
                a = 2
              {% end %}
            end
          )
          branch = Branch.of(nodes.macro_literal_nodes.first, nodes.def_nodes.first)
          branch.to_s.strip.should eq "a = 2"
        end
      end

      it "returns nil if branch does not exist" do
        nodes = as_nodes %(
          def method
            a = 2
          end
        )
        branch = Branch.of(nodes.assign_nodes.first, nodes.def_nodes.first)
        branch.should be_nil
      end
    end

    describe "#initialize" do
      it "creates new branch" do
        nodes = as_nodes %(
          if true
            a = 2
          end
        )
        branchable = Branchable.new nodes.if_nodes.first
        branch = Branch.new nodes.assign_nodes.first, branchable
        branch.node.should_not be_nil
      end
    end

    describe "delegation" do
      it "delegates to_s to node" do
        nodes = as_nodes %(
          if true
            a = 2
          end
        )
        branchable = Branchable.new nodes.if_nodes.first
        branch = Branch.new nodes.assign_nodes.first, branchable
        branch.to_s.should eq branch.node.to_s
      end

      it "delegates locations to node" do
        nodes = as_nodes %(
          if true
            a = 2
          end
        )
        branchable = Branchable.new nodes.if_nodes.first
        branch = Branch.new nodes.assign_nodes.first, branchable
        branch.location.should eq branch.node.location
        branch.end_location.should eq branch.node.end_location
      end
    end

    describe "#in_loop?" do
      it "returns true if branch is in a loop" do
        nodes = as_nodes %(
          while true
            a = 1
          end
        )
        branchable = Branchable.new nodes.while_nodes.first
        branch = Branch.new nodes.assign_nodes.first, branchable
        branch.in_loop?.should be_true
      end

      it "returns false if branch is not in a loop" do
        nodes = as_nodes %(
          if a > 2
            a = 1
          end
        )
        branchable = Branchable.new nodes.if_nodes.first
        branch = Branch.new nodes.assign_nodes.first, branchable
        branch.in_loop?.should be_false
      end
    end
  end
end
