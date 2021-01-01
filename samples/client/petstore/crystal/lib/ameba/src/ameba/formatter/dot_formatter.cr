require "./util"

module Ameba::Formatter
  # A formatter that shows a progress of inspection in a terminal using dots.
  # It is similar to Crystal's dot formatter for specs.
  class DotFormatter < BaseFormatter
    include Util

    @started_at : Time?
    @mutex = Thread::Mutex.new

    # Reports a message when inspection is started.
    def started(sources)
      @started_at = Time.utc # Time.monotonic

      output << started_message(sources.size)
    end

    # Reports a result of the inspection of a corresponding source.
    def source_finished(source : Source)
      sym = source.valid? ? ".".colorize(:green) : "F".colorize(:red)
      @mutex.synchronize { output << sym }
    end

    # Reports a message when inspection is finished.
    def finished(sources)
      output.flush
      output << "\n\n"

      show_affected_code = !config[:without_affected_code]?
      failed_sources = sources.reject &.valid?

      failed_sources.each do |source|
        source.issues.each do |issue|
          next if issue.disabled?
          next if (location = issue.location).nil?

          output << "#{location}\n".colorize(:cyan)
          output << "[#{issue.rule.severity.symbol}] #{issue.rule.name}: #{issue.message}\n".colorize(:red)

          if show_affected_code && (code = affected_code(source, location))
            output << code.colorize(:default)
          end

          output << "\n"
        end
      end

      output << finished_in_message(@started_at, Time.utc) # Time.monotonic
      output << final_message(sources, failed_sources)
    end

    private def started_message(size)
      if size == 1
        "Inspecting 1 file.\n\n".colorize(:default)
      else
        "Inspecting #{size} files.\n\n".colorize(:default)
      end
    end

    private def finished_in_message(started, finished)
      if started && finished
        "Finished in #{to_human(finished - started)} \n\n".colorize(:default)
      end
    end

    private def to_human(span : Time::Span)
      total_milliseconds = span.total_milliseconds
      if total_milliseconds < 1
        return "#{(span.total_milliseconds * 1_000).round.to_i} microseconds"
      end

      total_seconds = span.total_seconds
      if total_seconds < 1
        return "#{span.total_milliseconds.round(2)} milliseconds"
      end

      if total_seconds < 60
        return "#{total_seconds.round(2)} seconds"
      end

      minutes = span.minutes
      seconds = span.seconds
      "#{minutes}:#{seconds < 10 ? "0" : ""}#{seconds} minutes"
    end

    private def final_message(sources, failed_sources)
      total = sources.size
      failures = failed_sources.map { |f| f.issues.size }.sum
      color = failures == 0 ? :green : :red
      s = failures != 1 ? "s" : ""

      "#{total} inspected, #{failures} failure#{s}.\n".colorize color
    end
  end
end
