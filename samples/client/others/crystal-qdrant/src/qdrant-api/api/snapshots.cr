require "json"

module Qdrant::Api
  class Snapshots
    def initialize(@conn : Connection); end

    # Create storage snapshot Create new snapshot of the whole storage
    def create(*, wait : Bool? = nil) : Response(Qdrant::Api::CreateSnapshot200Response)
      @conn.request(Qdrant::Api::CreateSnapshot200Response,
        method: :POST,
        path: "/snapshots",
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete storage snapshot Delete snapshot of the whole storage
    def delete(snapshot_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/snapshots/{snapshot_name}".sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Download storage snapshot Download specified snapshot of the whole storage as a file
    def get(snapshot_name : String) : Response(::File)
      @conn.request(::File,
        method: :GET,
        path: "/snapshots/{snapshot_name}".sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        accept: %w[application/json application/octet-stream],
        auth: %w[api-key bearerAuth])
    end

    # List of storage snapshots Get list of snapshots of the whole storage
    def list() : Response(Qdrant::Api::ListSnapshots200Response)
      @conn.request(Qdrant::Api::ListSnapshots200Response,
        method: :GET,
        path: "/snapshots",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
