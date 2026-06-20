require "json"

module Qdrant::Api
  class Healthz
    def initialize(@conn : Connection); end

    # Kubernetes healthz endpoint An endpoint for health checking used in Kubernetes.
    def list() : Response(String)
      @conn.request(String,
        method: :GET,
        path: "/healthz",
        accept: %w[text/plain],
        raw: true,
        auth: %w[api-key bearerAuth])
    end
  end

end
