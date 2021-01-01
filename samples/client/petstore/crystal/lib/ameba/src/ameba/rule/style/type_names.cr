module Ameba::Rule::Style
  # A rule that enforces type names in camelcase manner.
  #
  # For example, these are considered valid:
  #
  # ```
  # class ParseError < Exception
  # end
  #
  # module HTTP
  #   class RequestHandler
  #   end
  # end
  #
  # alias NumericValue = Float32 | Float64 | Int32 | Int64
  #
  # lib LibYAML
  # end
  #
  # struct TagDirective
  # end
  #
  # enum Time::DayOfWeek
  # end
  # ```
  #
  # And these are invalid type names
  #
  # ```
  # class My_class
  # end
  #
  # module HTT_p
  # end
  #
  # alias Numeric_value = Int32
  #
  # lib Lib_YAML
  # end
  #
  # struct Tag_directive
  # end
  #
  # enum Time_enum::Day_of_week
  # end
  # ```
  #
  # YAML configuration example:
  #
  # ```
  # Style/TypeNames:
  #   Enabled: true
  # ```
  #
  struct TypeNames < Base
    properties do
      description "Enforces type names in camelcase manner"
    end

    MSG = "Type name should be camelcased: %s, but it was %s"

    private def check_node(source, node)
      name = node.name.to_s
      expected = name.camelcase
      return if expected == name

      issue_for node, MSG % {expected, name}
    end

    def test(source, node : Crystal::ClassDef)
      check_node(source, node)
    end

    def test(source, node : Crystal::Alias)
      check_node(source, node)
    end

    def test(source, node : Crystal::LibDef)
      check_node(source, node)
    end

    def test(source, node : Crystal::EnumDef)
      check_node(source, node)
    end

    def test(source, node : Crystal::ModuleDef)
      check_node(source, node)
    end
  end
end
