module Kemal
  # All loggers must inherit from `Kemal::BaseLogHandler`.
  abstract class BaseLogHandler
    include HTTP::Handler

    abstract def call(context : HTTP::Server::Context)
    abstract def write(message : String)
  end
end
