require "json"

module Qdrant::Api
  class Root
    def initialize(@conn : Connection); end

    # Returns information about the running Qdrant instance Returns information about the running Qdrant instance like version and commit id
    def list() : Response(Qdrant::Api::VersionInfo)
      @conn.request(Qdrant::Api::VersionInfo,
        method: :GET,
        path: "/",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
