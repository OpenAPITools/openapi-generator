require "json"

module Qdrant::Api
  class Livez
    def initialize(@conn : Connection); end

    # Kubernetes livez endpoint An endpoint for health checking used in Kubernetes.
    def list() : Response(String)
      @conn.request(String,
        method: :GET,
        path: "/livez",
        accept: %w[text/plain],
        raw: true,
        auth: %w[api-key bearerAuth])
    end
  end

end
