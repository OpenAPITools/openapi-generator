module Ameba::Rule::Lint
  # A rule that disallows useless conditions in when clause
  # where it is guaranteed to always return the same result.
  #
  # For example, this is considered invalid:
  #
  # ```
  # case
  # when utc?
  #   io << " UTC"
  # when local?
  #   Format.new(" %:z").format(self, io) if local?
  # end
  # ```
  #
  # And has to be written as the following:
  #
  # ```
  # case
  # when utc?
  #   io << " UTC"
  # when local?
  #   Format.new(" %:z").format(self, io)
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Lint/UselessConditionInWhen:
  #   Enabled: true
  # ```
  #
  struct UselessConditionInWhen < Base
    properties do
      description "Disallows useless conditions in when"
    end

    MSG = "Useless condition in when detected"

    # TODO: condition.cond may be a complex ASTNode with
    # useless inner conditions. We might need to improve this
    # simple implementation in future.
    protected def check_node(source, when_node, cond)
      cond_s = cond.to_s
      return if when_node
                  .conds
                  .map(&.to_s)
                  .none? { |c| c == cond_s }

      issue_for cond, MSG
    end

    def test(source, node : Crystal::When)
      ConditionInWhenVisitor.new self, source, node
    end

    # :nodoc:
    private class ConditionInWhenVisitor < Crystal::Visitor
      @source : Source
      @rule : UselessConditionInWhen
      @parent : Crystal::When

      def initialize(@rule, @source, @parent)
        @parent.accept self
      end

      def visit(node : Crystal::If)
        @rule.check_node(@source, @parent, node.cond)
        true
      end

      def visit(node : Crystal::Unless)
        @rule.check_node(@source, @parent, node.cond)
        true
      end

      def visit(node : Crystal::ASTNode)
        true
      end
    end
  end
end
