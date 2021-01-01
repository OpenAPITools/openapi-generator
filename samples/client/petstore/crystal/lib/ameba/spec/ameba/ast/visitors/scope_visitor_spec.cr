require "../../../spec_helper"

module Ameba::AST
  describe ScopeVisitor do
    it "creates a scope for the def" do
      rule = ScopeRule.new
      ScopeVisitor.new rule, Source.new %(
        def method
        end
      )
      rule.scopes.size.should eq 1
    end

    it "creates a scope for the proc" do
      rule = ScopeRule.new
      ScopeVisitor.new rule, Source.new %(
        -> {}
      )
      rule.scopes.size.should eq 1
    end

    it "creates a scope for the block" do
      rule = ScopeRule.new
      ScopeVisitor.new rule, Source.new %(
        3.times {}
      )
      rule.scopes.size.should eq 2
    end

    context "inner scopes" do
      it "creates scope for block inside def" do
        rule = ScopeRule.new
        ScopeVisitor.new rule, Source.new %(
          def method
            3.times {}
          end
        )
        rule.scopes.size.should eq 2
        rule.scopes.last.outer_scope.should_not be_nil
        rule.scopes.first.outer_scope.should eq rule.scopes.last
      end

      it "creates scope for block inside block" do
        rule = ScopeRule.new
        ScopeVisitor.new rule, Source.new %(
          3.times do
            2.times {}
          end
        )
        rule.scopes.size.should eq 3
        inner_block = rule.scopes.first
        outer_block = rule.scopes.last
        inner_block.outer_scope.should_not eq outer_block
        outer_block.outer_scope.should be_nil
      end
    end
  end
end
