require "../../ameba"
require "option_parser"

# :nodoc:
module Ameba::Cli
  extend self

  def run(args = ARGV)
    opts = parse_args args
    config = Config.load opts.config, opts.colors?
    config.globs = opts.globs.not_nil! if opts.globs
    config.severity = opts.fail_level.not_nil! if opts.fail_level

    configure_formatter(config, opts)
    configure_rules(config, opts)

    runner = Ameba.run(config)

    if location = opts.location_to_explain
      runner.explain(location)
    else
      exit 1 unless runner.success?
    end
  rescue e
    puts "Error: #{e.message}"
    exit 255
  end

  private class Opts
    property config = Config::PATH
    property formatter : Symbol | String | Nil
    property globs : Array(String)?
    property only : Array(String)?
    property except : Array(String)?
    property location_to_explain : NamedTuple(file: String, line: Int32, column: Int32)?
    property fail_level : Severity?
    property? all = false
    property? colors = true
    property? without_affected_code = false
  end

  def parse_args(args, opts = Opts.new)
    OptionParser.parse(args) do |parser|
      parser.banner = "Usage: ameba [options] [file1 file2 ...]"

      parser.on("-v", "--version", "Print version") { print_version }
      parser.on("-h", "--help", "Show this help") { show_help parser }
      parser.on("-s", "--silent", "Disable output") { opts.formatter = :silent }
      parser.unknown_args do |f|
        if f.size == 1 && f.first =~ /.+:\d+:\d+/
          configure_explain_opts(f.first, opts)
        else
          opts.globs = f if f.any?
        end
      end

      parser.on("-c", "--config PATH",
        "Specify a configuration file") do |path|
        opts.config = path unless opts.config.empty?
      end

      parser.on("-f", "--format FORMATTER",
        "Choose an output formatter: #{Config.formatter_names}") do |formatter|
        opts.formatter = formatter
      end

      parser.on("--only RULE1,RULE2,...",
        "Run only given rules (or groups)") do |rules|
        opts.only = rules.split ","
      end

      parser.on("--except RULE1,RULE2,...",
        "Disable the given rules (or groups)") do |rules|
        opts.except = rules.split ","
      end

      parser.on("--all", "Enables all available rules") do
        opts.all = true
      end

      parser.on("--gen-config",
        "Generate a configuration file acting as a TODO list") do
        opts.formatter = :todo
        opts.config = ""
      end

      parser.on("--fail-level SEVERITY", "Change the level of failure to exit. Defaults to Convention") do |level|
        opts.fail_level = Severity.parse(level)
      end

      parser.on("-e", "--explain PATH:line:column",
        "Explain an issue at a specified location") do |loc|
        configure_explain_opts(loc, opts)
      end

      parser.on("--without-affected-code",
        "Stop showing affected code while using a default formatter") do
        opts.without_affected_code = true
      end

      parser.on("--no-color", "Disable colors") do
        opts.colors = false
      end
    end

    opts
  end

  private def configure_rules(config, opts)
    if only = opts.only
      config.rules.map! { |r| r.enabled = false; r }
      config.update_rules(only, enabled: true)
    elsif opts.all?
      config.rules.map! { |r| r.enabled = true; r }
    end

    config.update_rules(opts.except, enabled: false)
  end

  private def configure_formatter(config, opts)
    if name = opts.formatter
      config.formatter = name
    end
    config.formatter.config[:without_affected_code] = opts.without_affected_code?
  end

  private def configure_explain_opts(loc, opts)
    location_to_explain = parse_explain_location(loc)
    opts.location_to_explain = location_to_explain
    opts.globs = [location_to_explain[:file]]
    opts.formatter = :silent
  end

  private def parse_explain_location(arg)
    location = arg.split(":", remove_empty: true).map &.strip
    raise ArgumentError.new unless location.size === 3
    file, line, column = location
    {file: file, line: line.to_i, column: column.to_i}
  rescue
    raise "location should have PATH:line:column format"
  end

  private def print_version
    puts VERSION
    exit 0
  end

  private def show_help(parser)
    puts parser
    exit 0
  end
end
