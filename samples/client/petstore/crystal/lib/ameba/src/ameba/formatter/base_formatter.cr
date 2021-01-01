require "./util"

# A module that utilizes Ameba's formatters.
module Ameba::Formatter
  # A base formatter for all formatters. It uses `output` IO
  # to report results and also implements stub methods for
  # callbacks in `Ameba::Runner#run` method.
  class BaseFormatter
    # TODO: allow other IOs
    getter output : IO::FileDescriptor | IO::Memory
    getter config = {} of Symbol => String | Bool

    def initialize(@output = STDOUT)
    end

    # Callback that indicates when inspecting is started.
    # A list of sources to inspect is passed as an argument.
    def started(sources); end

    # Callback that indicates when source inspection is finished.
    # A corresponding source is passed as an argument.
    def source_finished(source : Source); end

    # Callback that indicates when source inspection is finished.
    # A corresponding source is passed as an argument.
    def source_started(source : Source); end

    # Callback that indicates when inspection is finished.
    # A list of inspected sources is passed as an argument.
    def finished(sources); end
  end
end
