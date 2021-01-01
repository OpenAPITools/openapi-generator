module Ameba::Rule::Performance
  # This rule is used to identify usage of `first/last/first?/last?` calls that follow filters.
  #
  # For example, this is considered inefficient:
  #
  # ```
  # [-1, 0, 1, 2].select { |e| e > 0 }.first?
  # [-1, 0, 1, 2].select { |e| e > 0 }.last?
  # ```
  #
  # And can be written as this:
  #
  # ```
  # [-1, 0, 1, 2].find { |e| e > 0 }
  # [-1, 0, 1, 2].reverse_each.find { |e| e > 0 }
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Performance/FirstLastAfterFilter
  #   Enabled: true
  #   FilterNames:
  #     - select
  # ```
  #
  struct FirstLastAfterFilter < Base
    CALL_NAMES  = %w(first last first? last?)
    MSG         = "Use `find {...}` instead of `%s {...}.%s`"
    MSG_REVERSE = "Use `reverse_each.find {...}` instead of `%s {...}.%s`"

    properties do
      filter_names : Array(String) = %w(select)
      description "Identifies usage of `first/last/first?/last?` calls that follow filters."
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
      return unless CALL_NAMES.includes?(node.name) && (obj = node.obj)
      return if node.args.any?

      if node.block.nil? && obj.is_a?(Crystal::Call) &&
         filter_names.includes?(obj.name) && !obj.block.nil?
        message = node.name.includes?(CALL_NAMES.first) ? MSG : MSG_REVERSE
        issue_for obj.name_location, node.name_end_location, message % {obj.name, node.name}
      end
    end
  end
end
