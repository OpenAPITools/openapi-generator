require "json"

module Qdrant::Api
  class Cluster::Peer
    def initialize(@conn : Connection); end

    # Remove peer from the cluster Tries to remove peer from the cluster. Will return an error if peer has shards on it.
    def delete(peer_id : Int32, *, force : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/cluster/peer/{peer_id}".sub("{peer_id}", Qdrant::Api.enc(peer_id)),
        query: { "force" => force },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
