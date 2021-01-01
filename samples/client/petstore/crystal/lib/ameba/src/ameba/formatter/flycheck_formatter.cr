module Ameba::Formatter
  class FlycheckFormatter < BaseFormatter
    @mutex = Mutex.new

    def source_finished(source : Source)
      source.issues.each do |e|
        next if e.disabled?
        if loc = e.location
          @mutex.synchronize do
            output.printf "%s:%d:%d: %s: [%s] %s\n",
              source.path, loc.line_number, loc.column_number, e.rule.severity.symbol,
              e.rule.name, e.message.gsub("\n", " ")
          end
        end
      end
    end
  end
end
