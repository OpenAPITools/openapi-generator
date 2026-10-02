require "json"

module Qdrant::Api
  class Collections
    def initialize(@conn : Connection); end

    # Update aliases of the collections
    def aliases(change_aliases_operation : Qdrant::Api::ChangeAliasesOperation? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/aliases",
        body: change_aliases_operation,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # List aliases for collection Get list of all aliases for a collection
    def aliases_get(collection_name : String) : Response(Qdrant::Api::GetCollectionAliases200Response)
      @conn.request(Qdrant::Api::GetCollectionAliases200Response,
        method: :GET,
        path: "/collections/{collection_name}/aliases".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Collection cluster info Get cluster information for a collection
    def cluster(collection_name : String) : Response(Qdrant::Api::CollectionClusterInfo200Response)
      @conn.request(Qdrant::Api::CollectionClusterInfo200Response,
        method: :GET,
        path: "/collections/{collection_name}/cluster".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Collection cluster info Get cluster information for a collection
    def cluster_get(collection_name : String) : Response(Qdrant::Api::CollectionClusterInfo200Response)
      @conn.request(Qdrant::Api::CollectionClusterInfo200Response,
        method: :GET,
        path: "/collections/{collection_name}/cluster".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Update collection cluster setup
    def cluster_post(collection_name : String, cluster_operations : Qdrant::Api::ClusterOperations? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/cluster".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: cluster_operations,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Update collection cluster setup
    def cluster_post_1(collection_name : String, cluster_operations : Qdrant::Api::ClusterOperations? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :POST,
        path: "/collections/{collection_name}/cluster".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: cluster_operations,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete collection Drop collection and all associated data
    def delete(collection_name : String, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :DELETE,
        path: "/collections/{collection_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        query: { "timeout" => timeout },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Check the existence of a collection Returns \&quot;true\&quot; if the given collection name exists, and \&quot;false\&quot; otherwise
    def exists(collection_name : String) : Response(Qdrant::Api::CollectionExists200Response)
      @conn.request(Qdrant::Api::CollectionExists200Response,
        method: :GET,
        path: "/collections/{collection_name}/exists".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Facet a payload key with a given filter. Count points that satisfy the given filter for each unique value of a payload key.
    def facet(collection_name : String, facet_request : Qdrant::Api::FacetRequest? = nil, *, timeout : Int32? = nil, consistency : Qdrant::Api::ReadConsistency? = nil) : Response(Qdrant::Api::Facet200Response)
      @conn.request(Qdrant::Api::Facet200Response,
        method: :POST,
        path: "/collections/{collection_name}/facet".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: facet_request,
        query: { "timeout" => timeout, "consistency" => consistency },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Collection info Get detailed information about specified existing collection
    def get(collection_name : String) : Response(Qdrant::Api::GetCollection200Response)
      @conn.request(Qdrant::Api::GetCollection200Response,
        method: :GET,
        path: "/collections/{collection_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # List collections Get list name of all existing collections
    def list() : Response(Qdrant::Api::GetCollections200Response)
      @conn.request(Qdrant::Api::GetCollections200Response,
        method: :GET,
        path: "/collections",
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Update collection parameters Update parameters of the existing collection
    def partial_update(collection_name : String, update_collection : Qdrant::Api::UpdateCollection? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PATCH,
        path: "/collections/{collection_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: update_collection,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Create collection Create new collection with given parameters
    def update(collection_name : String, create_collection : Qdrant::Api::CreateCollection? = nil, *, timeout : Int32? = nil) : Response(Qdrant::Api::CreateShardKey200Response)
      @conn.request(Qdrant::Api::CreateShardKey200Response,
        method: :PUT,
        path: "/collections/{collection_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: create_collection,
        query: { "timeout" => timeout },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
