require "yaml"
require "./glob_utils"

# A configuration entry for `Ameba::Runner`.
#
# Config can be loaded from configuration YAML file and adjusted.
#
# ```
# config = Config.load
# config.formatter = my_formatter
# ```
#
# By default config loads `.ameba.yml` file in a current directory.
#
class Ameba::Config
  include GlobUtils

  AVAILABLE_FORMATTERS = {
    progress: Formatter::DotFormatter,
    todo:     Formatter::TODOFormatter,
    flycheck: Formatter::FlycheckFormatter,
    silent:   Formatter::BaseFormatter,
    disabled: Formatter::DisabledFormatter,
    json:     Formatter::JSONFormatter,
  }

  PATH = ".ameba.yml"

  DEFAULT_GLOBS = %w(
    **/*.cr
    !lib
  )

  setter formatter : Formatter::BaseFormatter?
  getter rules : Array(Rule::Base)
  property severity = Severity::Convention

  # Returns a list of paths (with wildcards) to files.
  # Represents a list of sources to be inspected.
  # If globs are not set, it will return default list of files.
  #
  # ```
  # config = Ameba::Config.load
  # config.globs = ["**/*.cr"]
  # config.globs
  # ```
  property globs : Array(String)

  # Represents a list of paths to exclude from globs.
  # Can have wildcards.
  #
  # ```
  # config = Ameba::Config.load
  # config.excluded = ["spec", "src/server/*.cr"]
  # ```
  property excluded : Array(String)

  @rule_groups : Hash(String, Array(Rule::Base))

  # Creates a new instance of `Ameba::Config` based on YAML parameters.
  #
  # `Config.load` uses this constructor to instantiate new config by YAML file.
  protected def initialize(config : YAML::Any)
    @rules = Rule.rules.map &.new(config).as(Rule::Base)
    @rule_groups = @rules.group_by &.group
    @excluded = load_array_section(config, "Excluded")
    @globs = load_array_section(config, "Globs", DEFAULT_GLOBS)

    self.formatter = load_formatter_name(config)
  end

  # Loads YAML configuration file by `path`.
  #
  # ```
  # config = Ameba::Config.load
  # ```
  #
  def self.load(path = PATH, colors = true)
    Colorize.enabled = colors
    content = File.exists?(path) ? File.read path : "{}"
    Config.new YAML.parse(content)
  rescue e
    raise "Config file is invalid: #{e.message}"
  end

  def self.formatter_names
    AVAILABLE_FORMATTERS.keys.join("|")
  end

  # Returns a list of sources matching globs and excluded sections.
  #
  # ```
  # config = Ameba::Config.load
  # config.sources # => list of default sources
  # config.globs = ["**/*.cr"]
  # config.excluded = ["spec"]
  # config.sources # => list of sources pointing to files found by the wildcards
  # ```
  #
  def sources
    (find_files_by_globs(globs) - find_files_by_globs(excluded))
      .map { |path| Source.new File.read(path), path }
  end

  # Returns a formatter to be used while inspecting files.
  # If formatter is not set, it will return default formatter.
  #
  # ```
  # config = Ameba::Config.load
  # config.formatter = custom_formatter
  # config.formatter
  # ```
  #
  def formatter
    @formatter ||= Formatter::DotFormatter.new
  end

  # Sets formatter by name.
  #
  # ```
  # config = Ameba::Config.load
  # config.formatter = :progress
  # ```
  #
  def formatter=(name : String | Symbol)
    if f = AVAILABLE_FORMATTERS[name]?
      @formatter = f.new
    else
      raise "Unknown formatter `#{name}`. Use one of #{Config.formatter_names}."
    end
  end

  # Updates rule properties.
  #
  # ```
  # config = Ameba::Config.load
  # config.update_rule "MyRuleName", enabled: false
  # ```
  #
  def update_rule(name, enabled = true, excluded = nil)
    index = @rules.index { |r| r.name == name }
    raise ArgumentError.new("Rule `#{name}` does not exist") unless index

    rule = @rules[index]
    rule.enabled = enabled
    rule.excluded = excluded
    @rules[index] = rule
  end

  # Updates rules properties.
  #
  # ```
  # config = Ameba::Config.load
  # config.update_rules %w(Rule1 Rule2), enabled: true
  # ```
  #
  # also it allows to update groups of rules:
  #
  # ```
  # config.update_rules %w(Group1 Group2), enabled: true
  # ```
  #
  def update_rules(names, **args)
    names.try &.each do |name|
      if group = @rule_groups[name]?
        group.each { |rule| update_rule(rule.name, **args) }
      else
        update_rule name, **args
      end
    end
  end

  private def load_formatter_name(config)
    name = config["Formatter"]?.try &.["Name"]?
    name ? name.to_s : nil
  end

  private def load_array_section(config, section_name, default = [] of String)
    case value = config[section_name]?
    when .nil?  then default
    when .as_s? then [value.to_s]
    when .as_a? then value.as_a.map(&.as_s)
    else
      raise "incorrect '#{section_name}' section in a config files"
    end
  end

  # :nodoc:
  module RuleConfig
    macro properties(&block)
      {% definitions = [] of NamedTuple %}
      {% if block.body.is_a? Assign %}
        {% definitions << {var: block.body.target, value: block.body.value} %}
      {% elsif block.body.is_a? Call %}
          {% definitions << {var: block.body.name, value: block.body.args.first} %}
      {% elsif block.body.is_a? TypeDeclaration %}
        {% definitions << {var: block.body.var, value: block.body.value, type: block.body.type} %}
      {% elsif block.body.is_a? Expressions %}
        {% for prop in block.body.expressions %}
          {% if prop.is_a? Assign %}
            {% definitions << {var: prop.target, value: prop.value} %}
          {% elsif prop.is_a? Call %}
            {% definitions << {var: prop.name, value: prop.args.first} %}
          {% elsif prop.is_a? TypeDeclaration %}
            {% definitions << {var: prop.var, value: prop.value, type: prop.type} %}
          {% end %}
        {% end %}
      {% end %}

      {% properties = {} of MacroId => NamedTuple %}
      {% for df in definitions %}
        {% name = df[:var].id %}
        {% key = name.camelcase.stringify %}
        {% value = df[:value] %}
        {% type = df[:type] %}
        {% converter = nil %}

        {% if key == "Severity" %}
          {% type = Severity %}
          {% converter = SeverityYamlConverter %}
        {% end %}

        {% if type == nil %}
          {% if value.is_a? BoolLiteral %}
            {% type = Bool %}
          {% elsif value.is_a? StringLiteral %}
            {% type = String %}
          {% elsif value.is_a? NumberLiteral %}
            {% if value.kind == :i32 %}
              {% type = Int32 %}
            {% elsif value.kind == :i64 %}
              {% type = Int64 %}
            {% elsif value.kind == :f32 %}
              {% type = Float32 %}
            {% elsif value.kind == :f64 %}
              {% type = Float64 %}
            {% end %}
          {% end %}

          {% type = Nil if type == nil %}
        {% end %}

        {% properties[name] = {key: key, default: value, type: type, converter: converter} %}

        @[YAML::Field(key: {{key}}, converter: {{converter}}, type: {{type}})]
        property {{name}} : {{type}} = {{value}}
      {% end %}

      {% if properties["enabled".id] == nil %}
        @[YAML::Field(key: "Enabled")]
        property enabled = true
      {% end %}

      {% if properties["severity".id] == nil %}
        {% default = @type.name.starts_with?("Ameba::Rule::Lint") ? "Ameba::Severity::Warning".id : "Ameba::Severity::Convention".id %}
        @[YAML::Field(key: "Severity", converter: Ameba::SeverityYamlConverter)]
        property severity = {{default}}
      {% end %}

      {% if properties["excluded".id] == nil %}
        @[YAML::Field(key: "Excluded")]
        property excluded : Array(String)?
      {% end %}
    end

    macro included
      macro inherited
        include YAML::Serializable
        include YAML::Serializable::Strict

        def self.new(config = nil)
          if (raw = config.try &.raw).is_a? Hash
            yaml = raw[rule_name]?.try &.to_yaml
          end
          from_yaml yaml || "{}"
        end
      end
    end
  end
end
