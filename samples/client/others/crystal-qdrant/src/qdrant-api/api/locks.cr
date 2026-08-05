require "json"

module Qdrant::Api
  class Locks
    def initialize(@conn : Connection); end

    # Set lock options Set lock options. If write is locked, all write operations and collection creation are forbidden. Returns previous lock options
    def create(locks_option : Qdrant::Api::LocksOption? = nil) : Response(Qdrant::Api::GetLocks200Response)
      @conn.request(Qdrant::Api::GetLocks200Response,
        method: :POST,
        path: "/locks",
        body: locks_option,
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Get lock options Get lock options. If write is locked, all write operations and collection creation are forbidden
    def list() : Response(Qdrant::Api::GetLocks200Response)
      @conn.request(Qdrant::Api::GetLocks200Response,
        method: :GET,
        path: "/locks",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
