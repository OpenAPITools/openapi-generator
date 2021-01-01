require "log"

module Crest
  abstract class Logger
    def self.new(filename : String)
      new(File.open(filename, "w"))
    end

    forward_missing_to @logger

    def initialize(@io : IO = STDOUT)
      backend = Log::IOBackend.new
      backend.io = @io

      @logger = Log.new("crest", backend, Log::Severity::Info)
      @logger.backend.as(Log::IOBackend).formatter = default_formatter

      @filters = [] of Array(String | Regex)
    end

    abstract def request(request : Crest::Request) : Nil
    abstract def response(response : Crest::Response) : Nil

    def default_formatter : Log::Formatter
      Log::Formatter.new do |entry, io|
        io << entry.source
        io << " | " << entry.timestamp.to_s("%F %T")
        io << " " << entry.message
      end
    end

    def info(message : String)
      @logger.info { apply_filters(message) }
    end

    def filter(patern : String | Regex, replacement : String)
      @filters.push([patern, replacement])
    end

    private def apply_filters(output : String) : String
      @filters.each do |f|
        patern = f[0]
        replacement = f[1]

        output = output.gsub(patern, replacement)
      end

      output
    end
  end
end

require "./loggers/*"
