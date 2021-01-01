require "../../../spec_helper"

module Ameba::AST
  describe CountingVisitor do
    describe "#visit" do
      it "allow to visit ASTNode" do
        node = Crystal::Parser.new("").parse
        visitor = CountingVisitor.new node
        node.accept visitor
      end
    end

    describe "#count" do
      it "is 1 for an empty method" do
        node = Crystal::Parser.new("def hello; end").parse
        visitor = CountingVisitor.new node

        visitor.count.should eq 1
      end

      it "is 1 if there is Macro::For" do
        code = %(
          def initialize()
            {% for c in ALL_NODES %}
              true || false
            {% end %}
          end
        )
        node = Crystal::Parser.new(code).parse
        visitor = CountingVisitor.new node
        visitor.count.should eq 1
      end

      it "is 1 if there is Macro::If" do
        code = %(
          def initialize()
            {% if foo.bar? %}
              true || false
            {% end %}
          end
        )
        node = Crystal::Parser.new(code).parse
        visitor = CountingVisitor.new node
        visitor.count.should eq 1
      end

      {% for pair in [
                       {code: "if true; end", description: "conditional"},
                       {code: "while true; end", description: "while loop"},
                       {code: "until 1 < 2; end", description: "until loop"},
                       {code: "begin; rescue; end", description: "rescue"},
                       {code: "case 1 when 1; end", description: "when"},
                       {code: "true || false", description: "or"},
                       {code: "true && false", description: "and"},
                     ] %}
        it "increases count for every {{ pair[:description].id }}" do
          node = Crystal::Parser.new("def hello; {{ pair[:code].id }} end").parse
          visitor = CountingVisitor.new node

          visitor.count.should eq 2
        end
      {% end %}
    end
  end
end
