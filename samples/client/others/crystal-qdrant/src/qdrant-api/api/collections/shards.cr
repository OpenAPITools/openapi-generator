require "json"

module Qdrant::Api
  class Collections::Shards
    def initialize(@conn : Connection); end

    # Create shard key
    def bulk_update(collection_name : String, create_sharding_key : Qdrant::Api::CreateShardingKey? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}/shards".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: create_sharding_key,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Create shard key
    def bulk_update_put(collection_name : String, create_sharding_key : Qdrant::Api::CreateShardingKey? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}/shards".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: create_sharding_key,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete shard key
    def delete(collection_name : String, drop_sharding_key : Qdrant::Api::DropShardingKey? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/shards/delete".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: drop_sharding_key,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete shard key
    def delete_post(collection_name : String, drop_sharding_key : Qdrant::Api::DropShardingKey? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/shards/delete".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: drop_sharding_key,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # List shards snapshots for a collection Get list of snapshots for a shard of a collection
    def snapshots(collection_name : String, shard_id : Int32) : Response(Qdrant::Api::ListSnapshots200Response)
      @conn.request(Qdrant::Api::ListSnapshots200Response,
        method: :GET,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete shard snapshot Delete snapshot of a shard for a collection
    def snapshots_delete(collection_name : String, shard_id : Int32, snapshot_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete shard snapshot Delete snapshot of a shard for a collection
    def snapshots_delete_1(collection_name : String, shard_id : Int32, snapshot_name : String, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # List shards snapshots for a collection Get list of snapshots for a shard of a collection
    def snapshots_get(collection_name : String, shard_id : Int32) : Response(Qdrant::Api::ListSnapshots200Response)
      @conn.request(Qdrant::Api::ListSnapshots200Response,
        method: :GET,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Download collection snapshot Download specified snapshot of a shard from a collection as a file
    def snapshots_get_1(collection_name : String, shard_id : Int32, snapshot_name : String) : Response(::File)
      @conn.request(::File,
        method: :GET,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        accept: %w[application/json application/octet-stream],
        auth: %w[api-key bearerAuth])
    end

    # Download collection snapshot Download specified snapshot of a shard from a collection as a file
    def snapshots_get_2(collection_name : String, shard_id : Int32, snapshot_name : String) : Response(::File)
      @conn.request(::File,
        method: :GET,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/{snapshot_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)).sub("{snapshot_name}", Qdrant::Api.enc(snapshot_name)),
        accept: %w[application/json application/octet-stream],
        auth: %w[api-key bearerAuth])
    end

    # Create shard snapshot Create new snapshot of a shard for a collection
    def snapshots_post(collection_name : String, shard_id : Int32, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateSnapshot200Response)
      @conn.request(Qdrant::Api::CreateSnapshot200Response,
        method: :POST,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Create shard snapshot Create new snapshot of a shard for a collection
    def snapshots_post_1(collection_name : String, shard_id : Int32, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateSnapshot200Response)
      @conn.request(Qdrant::Api::CreateSnapshot200Response,
        method: :POST,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        query: { "wait" => wait },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover from a snapshot Recover shard of a local collection data from a snapshot. This will overwrite any data, stored in this shard, for the collection.
    def snapshots_recover(collection_name : String, shard_id : Int32, shard_snapshot_recover : Qdrant::Api::ShardSnapshotRecover? = nil, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/recover".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        body: shard_snapshot_recover,
        query: { "wait" => wait },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover from a snapshot Recover shard of a local collection data from a snapshot. This will overwrite any data, stored in this shard, for the collection.
    def snapshots_recover_put(collection_name : String, shard_id : Int32, shard_snapshot_recover : Qdrant::Api::ShardSnapshotRecover? = nil, *, wait : Bool? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/recover".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        body: shard_snapshot_recover,
        query: { "wait" => wait },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover shard from an uploaded snapshot Recover shard of a local collection from an uploaded snapshot. This will overwrite any data, stored on this node, for the collection shard.
    def snapshots_upload(collection_name : String, shard_id : Int32, snapshot : ::File? = nil, *, wait : Bool? = nil, priority : Qdrant::Api::SnapshotPriority? = nil, checksum : String? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/upload".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        query: { "wait" => wait, "priority" => priority, "checksum" => checksum },
        form: Hash(String, Crest::ParamsValue){ "snapshot" => snapshot },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Recover shard from an uploaded snapshot Recover shard of a local collection from an uploaded snapshot. This will overwrite any data, stored on this node, for the collection shard.
    def snapshots_upload_post(collection_name : String, shard_id : Int32, snapshot : ::File? = nil, *, wait : Bool? = nil, priority : Qdrant::Api::SnapshotPriority? = nil, checksum : String? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/shards/{shard_id}/snapshots/upload".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{shard_id}", Qdrant::Api.enc(shard_id)),
        query: { "wait" => wait, "priority" => priority, "checksum" => checksum },
        form: Hash(String, Crest::ParamsValue){ "snapshot" => snapshot },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
