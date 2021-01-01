module Ameba::Rule::Performance
  # This rule is used to identify usage of `size` calls that follow filter.
  #
  # For example, this is considered invalid:
  #
  # ```
  # [1, 2, 3].select { |e| e > 2 }.size
  # [1, 2, 3].reject { |e| e < 2 }.size
  # [1, 2, 3].select(&.< 2).size
  # [0, 1, 2].select(&.zero?).size
  # [0, 1, 2].reject(&.zero?).size
  # ```
  #
  # And it should be written as this:
  #
  # ```
  # [1, 2, 3].count { |e| e > 2 }
  # [1, 2, 3].count { |e| e >= 2 }
  # [1, 2, 3].count(&.< 2)
  # [0, 1, 2].count(&.zero?)
  # [0, 1, 2].count(&.!= 0)
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Performance/SizeAfterFilter:
  #   Enabled: true
  #   FilterNames:
  #     - select
  #     - reject
  # ```
  #
  struct SizeAfterFilter < Base
    SIZE_NAME = "size"
    MSG       = "Use `count {...}` instead of `%s {...}.#{SIZE_NAME}`."

    properties do
      filter_names : Array(String) = %w(select reject)
      description "Identifies usage of `size` calls that follow filter"
    end

    def test(source)
      AST::NodeVisitor.new self, source, skip: [
        Crystal::Macro,
        Crystal::MacroExpression,
        Crystal::MacroIf,
        Crystal::MacroFor,
      ]
    end

    def test(source, node : Crystal::Call)
      return unless node.name == SIZE_NAME && (obj = node.obj)

      if obj.is_a?(Crystal::Call) &&
         filter_names.includes?(obj.name) && !obj.block.nil?
        issue_for obj.name_location, node.name_end_location, MSG % obj.name
      end
    end
  end
end
