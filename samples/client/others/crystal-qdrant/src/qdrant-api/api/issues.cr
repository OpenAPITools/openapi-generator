require "json"

module Qdrant::Api
  class Issues
    def initialize(@conn : Connection); end

    # Clear issues Removes all issues reported so far
    def bulk_destroy() : Response(Bool)
      @conn.request(Bool,
        method: :DELETE,
        path: "/issues",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Get issues Get a report of performance issues and configuration suggestions
    def list() : Response(JSON::Any)
      @conn.request(JSON::Any,
        method: :GET,
        path: "/issues",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
