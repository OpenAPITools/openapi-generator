require "json"

module Qdrant::Api
  class Telemetry
    def initialize(@conn : Connection); end

    # Collect telemetry data Collect telemetry data including app info, system info, collections info, cluster info, configs and statistics
    def list(*, anonymize : Bool? = nil) : Response(Qdrant::Api::Telemetry200Response)
      @conn.request(Qdrant::Api::Telemetry200Response,
        method: :GET,
        path: "/telemetry",
        query: { "anonymize" => anonymize },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
