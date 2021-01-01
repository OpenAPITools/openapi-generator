module Ameba::Rule::Style
  # A rule that disallows redundant return expressions.
  #
  # For example, this is considered invalid:
  #
  # ```
  # def foo
  #   return :bar
  # end
  # ```
  #
  # ```
  # def bar(arg)
  #   case arg
  #   when .nil?
  #     return "nil"
  #   when .blank?
  #     return "blank"
  #   else
  #     return "empty"
  #   end
  # end
  # ```
  #
  # And has to be written as the following:
  #
  # ```
  # def foo
  #   :bar
  # end
  # ```
  #
  # ```
  # def bar(arg)
  #   case arg
  #   when .nil?
  #     "nil"
  #   when .blank?
  #     "blank"
  #   else
  #     "empty"
  #   end
  # end
  # ```
  #
  # ### Configuration params
  #
  # 1. *allow_multi_return*, default: true
  #
  # Allows end-user to configure whether to report or not the return statements
  # which return tuple literals i.e.
  #
  # ```
  # def method(a, b)
  #   return a, b
  # end
  # ```
  #
  # If this param equals to `false`, the method above has to be written as:
  #
  # ```
  # def method(a, b)
  #   {a, b}
  # end
  # ```
  #
  # 2. *allow_empty_return*, default: true
  #
  # Allows end-user to configure whether to report or not the return statements
  # without arguments. Sometimes such returns are used to return the `nil` value explicitly.
  #
  # ```
  # def method
  #   @foo = :empty
  #   return
  # end
  # ```
  #
  # If this param equals to `false`, the method above has to be written as:
  #
  # ```
  # def method
  #   @foo = :empty
  #   nil
  # end
  # ```
  #
  # ### YAML config example
  #
  # ```
  # Style/RedundantReturn:
  #   Enabled: true
  #   AllowMutliReturn: true
  #   AllowEmptyReturn: true
  # ```
  struct RedundantReturn < Base
    properties do
      description "Reports redundant return expressions"
      allow_multi_return true
      allow_empty_return true
    end

    MSG = "Redundant `return` detected"

    def test(source, node : Crystal::Def)
      AST::RedundantControlExpressionVisitor.new(self, source, node.body)
    end

    def test(source, node : Crystal::Return, visitor : AST::RedundantControlExpressionVisitor)
      return if allow_multi_return && node.exp.is_a?(Crystal::TupleLiteral)
      return if allow_empty_return && (node.exp.nil? || node.exp.not_nil!.nop?)

      source.try &.add_issue self, node, MSG
    end
  end
end
