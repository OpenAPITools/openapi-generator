require "json"

module Qdrant::Api
  class Readyz
    def initialize(@conn : Connection); end

    # Kubernetes readyz endpoint An endpoint for health checking used in Kubernetes.
    def list() : Response(String)
      @conn.request(String,
        method: :GET,
        path: "/readyz",
        accept: %w[text/plain],
        raw: true,
        auth: %w[api-key bearerAuth])
    end
  end

end
