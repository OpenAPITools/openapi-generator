module Ameba::Rule::Style
  # A rule that enforces method names to be in underscored case.
  #
  # For example, these are considered valid:
  #
  # ```
  # class Person
  #   def first_name
  #   end
  #
  #   def date_of_birth
  #   end
  #
  #   def homepage_url
  #   end
  # end
  # ```
  #
  # And these are invalid method names:
  #
  # ```
  # class Person
  #   def firstName
  #   end
  #
  #   def date_of_Birth
  #   end
  #
  #   def homepageURL
  #   end
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/MethodNames:
  #   Enabled: true
  # ```
  #
  struct MethodNames < Base
    properties do
      description "Enforces method names to be in underscored case"
    end

    MSG = "Method name should be underscore-cased: %s, not %s"

    def test(source, node : Crystal::Def)
      return if (expected = node.name.underscore) == node.name

      line_number = node.location.try &.line_number
      column_number = node.name_location.try &.column_number

      return if line_number.nil? || column_number.nil?

      issue_for(
        {line_number, column_number},
        {line_number, column_number + node.name.size - 1},
        MSG % {expected, node.name}
      )
    end
  end
end
