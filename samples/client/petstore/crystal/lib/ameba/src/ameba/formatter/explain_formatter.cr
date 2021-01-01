require "./util"

module Ameba::Formatter
  # A formatter that shows the detailed explanation of the issue at
  # a specific location.
  class ExplainFormatter
    LINE_BREAK = "\n"
    HEADING    = "## "
    PREFIX     = " "

    include Util

    getter output : IO::FileDescriptor | IO::Memory
    getter location : Crystal::Location

    # Creates a new instance of ExplainFormatter.
    # Accepts *output* which indicates the io where the explanation will be wrtitten to.
    # Second argument is *location* which indicates the location to explain.
    #
    # ```
    # ExplainFormatter.new output,
    #   {file: path, line: line_number, column: column_number}
    # ```
    #
    def initialize(@output, loc)
      @location = Crystal::Location.new(loc[:file], loc[:line], loc[:column])
    end

    # Reports the explainations at the *@location*.
    def finished(sources)
      source = sources.find { |s| s.path == @location.filename }

      return unless source

      source.issues.each do |issue|
        if (location = issue.location) &&
           location.line_number == @location.line_number &&
           location.column_number == @location.column_number
          explain(source, issue)
        end
      end
    end

    private def explain(source, issue)
      rule = issue.rule

      output_title "ISSUE INFO"
      output_paragraph [
        issue.message.colorize(:red).to_s,
        @location.to_s.colorize(:cyan).to_s,
      ]

      if affected_code = affected_code(source, @location)
        output_title "AFFECTED CODE"
        output_paragraph affected_code
      end

      if rule.responds_to?(:description)
        output_title "RULE INFO"
        output_paragraph [rule.severity.to_s, rule.name, rule.description]
      end

      output_title "DETAILED DESCRIPTION"
      output_paragraph(rule.class.parsed_doc || "TO BE DONE...")
    end

    private def output_title(title)
      output << HEADING.colorize(:yellow) << title.colorize(:yellow) << LINE_BREAK
      output << LINE_BREAK
    end

    private def output_paragraph(paragraph : String)
      output_paragraph(paragraph.split(LINE_BREAK))
    end

    private def output_paragraph(paragraph : Array(String))
      paragraph.each do |line|
        output << PREFIX << line << LINE_BREAK
      end
      output << LINE_BREAK
    end
  end
end
