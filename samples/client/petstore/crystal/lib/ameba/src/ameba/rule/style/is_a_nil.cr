module Ameba::Rule::Style
  # A rule that disallows calls to `is_a?(Nil)` in favor of `nil?`.
  #
  # This is considered bad:
  #
  # ```
  # var.is_a? Nil
  # ```
  #
  # And needs to be written as:
  #
  # ```
  # var.nil?
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/IsANil:
  #   Enabled: true
  # ```
  #
  struct IsANil < Base
    properties do
      description "Disallows calls to `is_a?(Nil)` in favor of `nil?`"
    end

    MSG            = "Use `nil?` instead of `is_a?(Nil)`"
    PATH_NIL_NAMES = %w(Nil)

    def test(source, node : Crystal::IsA)
      return if node.nil_check?

      const = node.const
      issue_for const, MSG if const.is_a?(Crystal::Path) && const.names == PATH_NIL_NAMES
    end
  end
end
