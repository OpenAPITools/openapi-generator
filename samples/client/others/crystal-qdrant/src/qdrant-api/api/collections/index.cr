require "json"

module Qdrant::Api
  class Collections::Index
    def initialize(@conn : Connection); end

    # Create index for field in collection Create index for field in collection
    def bulk_update(collection_name : String, create_field_index : Qdrant::Api::CreateFieldIndex? = nil, *, wait : Bool? = nil, ordering : Qdrant::Api::WriteOrdering? = nil) : Response(Qdrant::Api::CreateFieldIndex200Response)
      @conn.request(Qdrant::Api::CreateFieldIndex200Response,
        method: :PUT,
        path: "/collections/{collection_name}/index".sub("{collection_name}", Qdrant::Api.enc(collection_name)),
        body: create_field_index,
        query: { "wait" => wait, "ordering" => ordering },
        accept: %w[application/json],
        content_type: %w[application/json],
        auth: %w[api-key bearerAuth])
    end

    # Delete index for field in collection Delete field index for collection
    def delete(collection_name : String, field_name : String, *, wait : Bool? = nil, ordering : Qdrant::Api::WriteOrdering? = nil) : Response(Qdrant::Api::CreateFieldIndex200Response)
      @conn.request(Qdrant::Api::CreateFieldIndex200Response,
        method: :DELETE,
        path: "/collections/{collection_name}/index/{field_name}".sub("{collection_name}", Qdrant::Api.enc(collection_name)).sub("{field_name}", Qdrant::Api.enc(field_name)),
        query: { "wait" => wait, "ordering" => ordering },
        accept: %w[application/json],
        auth: %w[api-key bearerAuth])
    end
  end

end
