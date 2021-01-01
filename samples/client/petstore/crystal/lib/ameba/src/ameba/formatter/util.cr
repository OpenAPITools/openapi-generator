module Ameba::Formatter
  module Util
    def affected_code(source, location, max_length = 100, placeholder = " ...", prompt = "> ")
      line, column = location.line_number, location.column_number
      affected_line = source.lines[line - 1]?

      return if affected_line.nil? || affected_line.strip.empty?

      if affected_line.size > max_length && column < max_length
        affected_line = affected_line[0, max_length - placeholder.size - 1] + placeholder
      end

      stripped = affected_line.lstrip
      position = column - (affected_line.size - stripped.size) + prompt.size

      String.build do |str|
        str << prompt << stripped << "\n"
        str << " " * (position - 1)
        str << "^".colorize(:yellow)
      end
    end
  end
end
