module Ameba::Rule::Style
  # A rule that enforces variable names to be in underscored case.
  #
  # For example, these variable names are considered valid:
  #
  # ```
  # var_name = 1
  # name = 2
  # _another_good_name = 3
  # ```
  #
  # And these are invalid variable names:
  #
  # ```
  # myBadNamedVar = 1
  # wrong_Name = 2
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/VariableNames:
  #   Enabled: true
  # ```
  #
  struct VariableNames < Base
    properties do
      description "Enforces variable names to be in underscored case"
    end

    MSG = "Var name should be underscore-cased: %s, not %s"

    private def check_node(source, node)
      return if (expected = node.name.underscore) == node.name

      issue_for node, MSG % {expected, node.name}
    end

    def test(source, node : Crystal::Var)
      check_node source, node
    end

    def test(source, node : Crystal::InstanceVar)
      check_node source, node
    end

    def test(source, node : Crystal::ClassVar)
      check_node source, node
    end
  end
end
