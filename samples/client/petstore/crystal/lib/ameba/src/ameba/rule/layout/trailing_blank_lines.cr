module Ameba::Rule::Layout
  # A rule that disallows trailing blank lines at the end of the source file.
  #
  # YAML configuration example:
  #
  # ```
  # Layout/TrailingBlankLines:
  #   Enabled: true
  # ```
  #
  struct TrailingBlankLines < Base
    properties do
      description "Disallows trailing blank lines"
    end

    MSG               = "Excessive trailing newline detected"
    MSG_FINAL_NEWLINE = "Trailing newline missing"

    def test(source)
      source_lines = source.lines
      return if source_lines.empty?

      last_source_line = source_lines.last
      source_lines_size = source_lines.size
      return if source_lines_size == 1 && last_source_line.empty?

      last_line_not_empty = !last_source_line.empty?
      if source_lines_size >= 1 && (source_lines.last(2).join.strip.empty? || last_line_not_empty)
        issue_for({source_lines_size - 1, 1}, last_line_not_empty ? MSG_FINAL_NEWLINE : MSG)
      end
    end
  end
end
