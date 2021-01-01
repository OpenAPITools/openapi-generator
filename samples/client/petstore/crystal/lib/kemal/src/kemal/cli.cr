require "option_parser"

module Kemal
  # Handles all the initialization from the command line.
  class CLI
    def initialize(args)
      @ssl_enabled = false
      @key_file = ""
      @cert_file = ""
      @config = Kemal.config
      if args
        parse args
      end
      configure_ssl
    end

    private def parse(args : Array(String))
      OptionParser.parse args do |opts|
        opts.on("-b HOST", "--bind HOST", "Host to bind (defaults to 0.0.0.0)") do |host_binding|
          @config.host_binding = host_binding
        end
        opts.on("-p PORT", "--port PORT", "Port to listen for connections (defaults to 3000)") do |opt_port|
          @config.port = opt_port.to_i
        end
        opts.on("-s", "--ssl", "Enables SSL") do
          @ssl_enabled = true
        end
        opts.on("--ssl-key-file FILE", "SSL key file") do |key_file|
          @key_file = key_file
        end
        opts.on("--ssl-cert-file FILE", "SSL certificate file") do |cert_file|
          @cert_file = cert_file
        end
        opts.on("-h", "--help", "Shows this help") do
          puts opts
          exit 0
        end
        @config.extra_options.try &.call(opts)
      end
    end

    private def configure_ssl
      {% if !flag?(:without_openssl) %}
        if @ssl_enabled
          abort "SSL Key Not Found" if !@key_file
          abort "SSL Certificate Not Found" if !@cert_file
          ssl = Kemal::SSL.new
          ssl.key_file = @key_file.not_nil!
          ssl.cert_file = @cert_file.not_nil!
          Kemal.config.ssl = ssl.context
        end
      {% end %}
    end
  end
end
