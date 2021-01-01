module Ameba::Formatter
  # A formatter that shows all disabled lines by inline directives.
  class DisabledFormatter < BaseFormatter
    def finished(sources)
      output << "Disabled rules using inline directives: \n\n"

      sources.each do |source|
        source.issues.select(&.disabled?).each do |e|
          if loc = e.location
            output << "#{source.path}:#{loc.line_number}".colorize(:cyan)
            output << " #{e.rule.name}\n"
          end
        end
      end
    end
  end
end
