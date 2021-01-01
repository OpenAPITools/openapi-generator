module Kemal
  # Uses `STDOUT` by default and handles the logging of request/response process time.
  class LogHandler < Kemal::BaseLogHandler
    def initialize(@io : IO = STDOUT)
    end

    def call(context : HTTP::Server::Context)
      elapsed_time = Time.measure { call_next(context) }
      elapsed_text = elapsed_text(elapsed_time)
      @io << Time.utc << ' ' << context.response.status_code << ' ' << context.request.method << ' ' << context.request.resource << ' ' << elapsed_text << '\n'
      @io.flush
      context
    end

    def write(message : String)
      @io << message
      @io.flush
      @io
    end

    private def elapsed_text(elapsed)
      millis = elapsed.total_milliseconds
      return "#{millis.round(2)}ms" if millis >= 1

      "#{(millis * 1000).round(2)}µs"
    end
  end
end
