require "json"

module Qdrant::Api
  class Collections::Snapshots
    def initialize(@conn : Connection); end

    # Create collection snapshot Create new snapshot for a collection
    def create(collection_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateSnapshot200Response)
      @conn.request(Qdrant::Api::CreateSnapshot200Response,
        method: :POST,
        path: "/collections/{collection_name}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Create collection snapshot Create new snapshot for a collection
    def create_post(collection_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateSnapshot200Response)
      @conn.request(Qdrant::Api::CreateSnapshot200Response,
        method: :POST,
        path: "/collections/{collection_name}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete collection snapshot Delete snapshot for a collection
    def delete(collection_name : String, snapshot_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/collections/{collection_name}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete collection snapshot Delete snapshot for a collection
    def delete_delete(collection_name : String, snapshot_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/collections/{collection_name}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Download collection snapshot Download specified snapshot from a collection as a file
    def get(collection_name : String, snapshot_name : String) : Response(::File)
      @conn.request(::File,
        method: :GET,
        path: "/collections/{collection_name}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        accept: %w[application/json application/octet-stream],
        auth: %w[api-key bearerAuth])
    end

    # Download collection snapshot Download specified snapshot from a collection as a file
    def get_get(collection_name : String, snapshot_name : String) : Response(::File)
      @conn.request(::File,
        method: :GET,
        path: "/collections/{collection_name}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        accept: %w[application/json application/octet-stream],
        auth: %w[api-key bearerAuth])
    end

    # List collection snapshots Get list of snapshots for a collection
    def list(collection_name : String) : Response(Qdrant::Api::ListSnapshots200Response)
      @conn.request(Qdrant::Api::ListSnapshots200Response,
        method: :GET,
        path: "/collections/{collection_name}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # List collection snapshots Get list of snapshots for a collection
    def list_get(collection_name : String) : Response(Qdrant::Api::ListSnapshots200Response)
      @conn.request(Qdrant::Api::ListSnapshots200Response,
        method: :GET,
        path: "/collections/{collection_name}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover from a snapshot Recover local collection data from a snapshot. This will overwrite any data, stored on this node, for the collection. If collection does not exist - it will be created.
    def recover(collection_name : String, snapshot_recover : Qdrant::Api::SnapshotRecover? = nil, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}/snapshots/recover".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: snapshot_recover,
        query: { "wait" => wait },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover from a snapshot Recover local collection data from a snapshot. This will overwrite any data, stored on this node, for the collection. If collection does not exist - it will be created.
    def recover_put(collection_name : String, snapshot_recover : Qdrant::Api::SnapshotRecover? = nil, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}/snapshots/recover".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: snapshot_recover,
        query: { "wait" => wait },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover from an uploaded snapshot Recover local collection data from an uploaded snapshot. This will overwrite any data, stored on this node, for the collection. If collection does not exist - it will be created.
    def upload(collection_name : String, snapshot : ::File? = nil, *, wait : Bool? = nil, priority : Qdrant::Api::SnapshotPriority? = nil, checksum : String? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/snapshots/upload".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        query: { "wait" => wait, "priority" => priority, "checksum" => checksum },
        form: Hash(String, Crest::ParamsValue){ "snapshot" => snapshot },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover from an uploaded snapshot Recover local collection data from an uploaded snapshot. This will overwrite any data, stored on this node, for the collection. If collection does not exist - it will be created.
    def upload_post(collection_name : String, snapshot : ::File? = nil, *, wait : Bool? = nil, priority : Qdrant::Api::SnapshotPriority? = nil, checksum : String? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/snapshots/upload".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        query: { "wait" => wait, "priority" => priority, "checksum" => checksum },
        form: Hash(String, Crest::ParamsValue){ "snapshot" => snapshot },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
