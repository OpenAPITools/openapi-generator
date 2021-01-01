require "../../../spec_helper"

module Ameba::AST
  rule = DummyRule.new
  source = Source.new ""

  describe NodeVisitor do
    describe "visit" do
      it "allow to visit ASTNode" do
        visitor = NodeVisitor.new rule, source
        nodes = Crystal::Parser.new("").parse
        nodes.accept visitor
      end
    end
  end
end
