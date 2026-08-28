require "json"

module Qdrant::Api
  class Aliases
    def initialize(@conn : Connection); end

    # List collections aliases Get list of all existing collections aliases
    def list() : Response(Qdrant::Api::GetCollectionAliases200Response)
      @conn.request(Qdrant::Api::GetCollectionAliases200Response,
        method: :GET,
        path: "/aliases",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
