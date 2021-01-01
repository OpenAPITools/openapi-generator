require "../../../spec_helper"

module Ameba::AST
  source = Source.new ""
  rule = RedundantControlExpressionRule.new

  describe RedundantControlExpressionVisitor do
    node = as_node %(
      a = 1
      b = 2
      return a + b
    )
    subject = RedundantControlExpressionVisitor.new(rule, source, node)

    it "assigns valid attributes" do
      subject.rule.should eq rule
      subject.source.should eq source
      subject.node.should eq node
    end

    it "fires a callback with a valid node" do
      rule.nodes.size.should eq 1
      rule.nodes.first.to_s.should eq "return a + b"
    end
  end
end
