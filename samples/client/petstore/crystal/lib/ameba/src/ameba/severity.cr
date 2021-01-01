module Ameba
  enum Severity
    Error
    Warning
    Convention

    # Returns a symbol uniquely indicating severity.
    #
    # ```
    # Severity::Warning.symbol # => 'W'
    # ```
    def symbol
      to_s[0]
    end

    # Creates Severity by the name.
    #
    # ```
    # Severity.parse("convention") # => Severity::Convention
    # Severity.parse("foo-bar")    # => Exception: Incorrect severity name
    # ```
    #
    def self.parse(name : String)
      super name
    rescue ArgumentError
      raise "Incorrect severity name #{name}. Try one of #{values}"
    end
  end

  # Converter for `YAML.mapping` which converts severity enum to and from YAML.
  class SeverityYamlConverter
    def self.from_yaml(ctx : YAML::ParseContext, node : YAML::Nodes::Node)
      unless node.is_a?(YAML::Nodes::Scalar)
        raise "Severity must be a scalar, not #{node.class}"
      end

      case value = node.value
      when String then Severity.parse(value)
      when Nil    then nil
      else
        raise "Incorrect severity: #{value}"
      end
    end

    def self.to_yaml(value : Severity, yaml : YAML::Nodes::Builder)
      yaml.scalar value
    end
  end
end
