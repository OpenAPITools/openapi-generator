require "../../spec_helper"
require "semantic_version"

module Ameba::AST
  struct Test
    include Util
  end

  subject = Test.new

  describe Util do
    describe "#literal?" do
      [
        Crystal::ArrayLiteral.new,
        Crystal::BoolLiteral.new(false),
        Crystal::CharLiteral.new('a'),
        Crystal::HashLiteral.new,
        Crystal::NamedTupleLiteral.new,
        Crystal::NilLiteral.new,
        Crystal::NumberLiteral.new(42),
        Crystal::RegexLiteral.new(Crystal::StringLiteral.new("")),
        Crystal::StringLiteral.new(""),
        Crystal::SymbolLiteral.new(""),
        Crystal::TupleLiteral.new([] of Crystal::ASTNode),
        Crystal::RangeLiteral.new(
          Crystal::NilLiteral.new,
          Crystal::NilLiteral.new,
          true),
      ].each do |literal|
        it "returns true if node is #{literal}" do
          subject.literal?(literal).should be_true
        end
      end

      it "returns false if node is not a literal" do
        subject.literal?(Crystal::Nop).should be_false
      end
    end

    describe "#node_source" do
      it "returns original source of the node" do
        s = %(
          a = 1
        )
        node = Crystal::Parser.new(s).parse
        source = subject.node_source node, s.split("\n")
        source.should eq ["a = 1"]
      end

      it "returns original source of multiline node" do
        s = %(
          if ()
            :ok
          end
        )
        node = Crystal::Parser.new(s).parse
        source = subject.node_source node, s.split("\n")
        source.should eq([
          "if ()",
          "            :ok",
          "          end",
        ])
      end

      it "does not report source of node which has incorrect location" do
        s = %q(
          module MyModule
            macro conditional_error_for_inline_callbacks
              \{%
                raise ""
              %}
            end

            macro before_save(x = nil)
            end
          end
        )
        node = as_nodes(s).nil_literal_nodes.first
        source = subject.node_source node, s.split("\n")

        if SemanticVersion.parse(Crystal::VERSION) <= SemanticVersion.parse("0.35.1")
          source.should be_nil
        else
          source.should eq %w(nil)
        end
      end
    end

    describe "#flow_command?" do
      it "returns true if this is return" do
        node = as_node("return 22")
        subject.flow_command?(node, false).should eq true
      end

      it "returns true if this is a break in a loop" do
        node = as_node("break")
        subject.flow_command?(node, true).should eq true
      end

      it "returns false if this is a break out of loop" do
        node = as_node("break")
        subject.flow_command?(node, false).should eq false
      end

      it "returns true if this is a next in a loop" do
        node = as_node("next")
        subject.flow_command?(node, true).should eq true
      end

      it "returns false if this is a next out of loop" do
        node = as_node("next")
        subject.flow_command?(node, false).should eq false
      end

      it "returns true if this is raise" do
        node = as_node("raise e")
        subject.flow_command?(node, false).should eq true
      end

      it "returns true if this is exit" do
        node = as_node("exit")
        subject.flow_command?(node, false).should eq true
      end

      it "returns true if this is abort" do
        node = as_node("abort")
        subject.flow_command?(node, false).should eq true
      end

      it "returns false otherwise" do
        node = as_node("foobar")
        subject.flow_command?(node, false).should eq false
      end
    end

    describe "#flow_expression?" do
      it "returns true if this is a flow command" do
        node = as_node("return")
        subject.flow_expression?(node, true).should eq true
      end

      it "returns true if this is if-else consumed by flow expressions" do
        node = as_node %(
          if foo
            return :foo
          else
            return :bar
          end
        )
        subject.flow_expression?(node, false).should eq true
      end

      it "returns true if this is unless-else consumed by flow expressions" do
        node = as_node %(
          unless foo
            return :foo
          else
            return :bar
          end
        )
        subject.flow_expression?(node).should eq true
      end

      it "returns true if this is case consumed by flow expressions" do
        node = as_node %(
          case
          when 1
            return 1
          when 2
            return 2
          else
            return 3
          end
        )
        subject.flow_expression?(node).should eq true
      end

      it "returns true if this is exception handler consumed by flow expressions" do
        node = as_node %(
          begin
            raise "exp"
          rescue e
            return e
          end
        )
        subject.flow_expression?(node).should eq true
      end

      it "returns true if this while consumed by flow expressions" do
        node = as_node %(
          while true
            return
          end
        )
        subject.flow_expression?(node).should eq true
      end

      it "returns false if this while with break" do
        node = as_node %(
          while true
            break
          end
        )
        subject.flow_expression?(node).should eq false
      end

      it "returns true if this until consumed by flow expressions" do
        node = as_node %(
          until false
            return
          end
        )
        subject.flow_expression?(node).should eq true
      end

      it "returns false if this until with break" do
        node = as_node %(
          until false
            break
          end
        )
        subject.flow_expression?(node).should eq false
      end

      it "returns true if this expressions consumed by flow expressions" do
        node = as_node %(
          exp1
          exp2
          return
        )
        subject.flow_expression?(node).should eq true
      end

      it "returns false otherwise" do
        node = as_node %(
          exp1
          exp2
        )
        subject.flow_expression?(node).should eq false
      end
    end

    describe "#raise?" do
      it "returns true if this is a raise method call" do
        node = as_node "raise e"
        subject.raise?(node).should eq true
      end

      it "returns false if it has a receiver" do
        node = as_node "obj.raise e"
        subject.raise?(node).should eq false
      end

      it "returns false if size of the arguments doesn't match" do
        node = as_node "raise"
        subject.raise?(node).should eq false
      end
    end

    describe "#exit?" do
      it "returns true if this is a exit method call" do
        node = as_node "exit"
        subject.exit?(node).should eq true
      end

      it "returns true if this is a exit method call with one argument" do
        node = as_node "exit 1"
        subject.exit?(node).should eq true
      end

      it "returns false if it has a receiver" do
        node = as_node "obj.exit"
        subject.exit?(node).should eq false
      end

      it "returns false if size of the arguments doesn't match" do
        node = as_node "exit 1, 1"
        subject.exit?(node).should eq false
      end
    end

    describe "#abort?" do
      it "returns true if this is an abort method call" do
        node = as_node "abort"
        subject.abort?(node).should eq true
      end

      it "returns true if this is an abort method call with one argument" do
        node = as_node "abort \"message\""
        subject.abort?(node).should eq true
      end

      it "returns true if this is an abort method call with two arguments" do
        node = as_node "abort \"message\", 1"
        subject.abort?(node).should eq true
      end

      it "returns false if it has a receiver" do
        node = as_node "obj.abort"
        subject.abort?(node).should eq false
      end

      it "returns false if size of the arguments doesn't match" do
        node = as_node "abort 1, 1, 1"
        subject.abort?(node).should eq false
      end
    end

    describe "#loop?" do
      it "returns true if this is a loop method call" do
        node = as_node "loop"
        subject.loop?(node).should eq true
      end

      it "returns false if it has a receiver" do
        node = as_node "obj.loop"
        subject.loop?(node).should eq false
      end

      it "returns false if size of the arguments doesn't match" do
        node = as_node "loop 1"
        subject.loop?(node).should eq false
      end
    end
  end
end
