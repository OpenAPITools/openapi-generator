module Ameba::Rule::Style
  # A rule that enforces constant names to be in screaming case.
  #
  # For example, these constant names are considered valid:
  #
  # ```
  # LUCKY_NUMBERS     = [3, 7, 11]
  # DOCUMENTATION_URL = "http://crystal-lang.org/docs"
  # ```
  #
  # And these are invalid names:
  #
  # ```
  # myBadConstant = 1
  # Wrong_NAME = 2
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/ConstantNames:
  #   Enabled: true
  # ```
  #
  struct ConstantNames < Base
    properties do
      description "Enforces constant names to be in screaming case"
    end

    MSG = "Constant name should be screaming-cased: %s, not %s"

    def test(source, node : Crystal::Assign)
      if (target = node.target).is_a? Crystal::Path
        name = target.names.first
        expected = name.upcase

        return if expected == name || name.camelcase == name

        issue_for target, MSG % {expected, name}
      end
    end
  end
end
