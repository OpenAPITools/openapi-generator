module Ameba
  # Represents a runner for inspecting sources files.
  # Holds a list of rules to do inspection based on,
  # list of sources to run inspection on and a formatter
  # to prepare a report.
  #
  # ```
  # config = Ameba::Config.load
  # runner = Ameba::Runner.new config
  # runner.run.success? # => true or false
  # ```
  #
  class Runner
    # A list of rules to do inspection based on.
    @rules : Array(Rule::Base)

    # A list of sources to run inspection on.
    getter sources : Array(Source)

    # A level of severity to be reported.
    @severity : Severity

    # A formatter to prepare report.
    @formatter : Formatter::BaseFormatter

    # A syntax rule which always inspects a source first
    @syntax_rule = Rule::Lint::Syntax.new

    # Checks for unneeded disable directives. Always inspects a source last
    @unneeded_disable_directive_rule : Rule::Base?

    # Instantiates a runner using a `config`.
    #
    # ```
    # config = Ameba::Config.load
    # config.files = files
    # config.formatter = formatter
    #
    # Ameba::Runner.new config
    # ```
    #
    def initialize(config : Config)
      @sources = config.sources
      @formatter = config.formatter
      @severity = config.severity
      @rules = config.rules.select(&.enabled).reject!(&.special?)

      @unneeded_disable_directive_rule =
        config.rules
          .find &.name.==(Rule::Lint::UnneededDisableDirective.rule_name)
    end

    # :nodoc:
    protected def initialize(@rules, @sources, @formatter, @severity)
    end

    # Performs the inspection. Iterates through all sources and test it using
    # list of rules. If a specific rule fails on a specific source, it adds
    # an issue to that source.
    #
    # This action also notifies formatter when inspection is started/finished,
    # and when a specific source started/finished to be inspected.
    #
    # ```
    # runner = Ameba::Runner.new config
    # runner.run # => returns runner again
    # ```
    #
    def run
      @formatter.started @sources
      channels = @sources.map { Channel(Exception?).new }
      @sources.each_with_index do |source, idx|
        channel = channels[idx]
        spawn do
          run_source(source)
        rescue e
          channel.send(e)
        else
          channel.send(nil)
        end
      end

      channels.each do |c|
        e = c.receive
        raise e unless e.nil?
      end

      self
    ensure
      @formatter.finished @sources
    end

    private def run_source(source)
      @formatter.source_started source

      if @syntax_rule.catch(source).valid?
        @rules.each do |rule|
          next if rule.excluded?(source)
          rule.test(source)
        end
        check_unneeded_directives(source)
      end

      @formatter.source_finished source
    end

    # Explains an issue at a specified *location*.
    #
    # Runner should perform inspection before doing the explain.
    # This is necessary to be able to find the issue at a specified location.
    #
    # ```
    # runner = Ameba::Runner.new config
    # runner.run
    # runner.explain({file: file, line: l, column: c})
    # ```
    #
    def explain(location, output = STDOUT)
      Formatter::ExplainFormatter.new(output, location).finished @sources
    end

    # Indicates whether the last inspection successful or not.
    # It returns true if no issues matching severity in sources found, false otherwise.
    #
    # ```
    # runner = Ameba::Runner.new config
    # runner.run
    # runner.success? # => true or false
    # ```
    #
    def success?
      @sources.all? do |source|
        source.issues
          .reject(&.disabled?)
          .none? { |issue| issue.rule.severity <= @severity }
      end
    end

    private def check_unneeded_directives(source)
      if (rule = @unneeded_disable_directive_rule) && rule.enabled
        rule.test(source)
      end
    end
  end
end
