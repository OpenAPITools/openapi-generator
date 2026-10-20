require "json"

module Qdrant::Api
  class Metrics
    def initialize(@conn : Connection); end

    # Collect Prometheus metrics data Collect metrics data including app info, collections info, cluster info and statistics
    def list(*, anonymize : Bool? = nil) : Response(String)
      @conn.request(String,
        method: :GET,
        path: "/metrics",
        query: { "anonymize" => anonymize },
        accept: %w[text/plain],
        raw: true,
        auth: %w[api-key bearerAuth])
    end
  end

end
