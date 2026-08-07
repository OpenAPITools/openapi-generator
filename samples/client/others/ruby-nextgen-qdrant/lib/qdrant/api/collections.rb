# frozen_string_literal: true

module Qdrant
  module Api
    class Collections
      def initialize(connection)
        @connection = connection
      end

      def aliases(timeout: nil, change_aliases_operation: nil)
        @connection.call(
          :POST,
          '/collections/aliases',
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: change_aliases_operation
        )
      end

      def aliases_get(collection_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/aliases'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::GetCollectionAliases200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def cluster(collection_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/cluster'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CollectionClusterInfo200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def cluster_post(collection_name:, timeout: nil, cluster_operations: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/cluster'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: cluster_operations
        )
      end

      def delete(collection_name:, timeout: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :DELETE,
          '/collections/{collection_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout }
        )
      end

      def exists(collection_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/exists'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CollectionExists200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def facet(collection_name:, timeout: nil, consistency: nil, facet_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/facet'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::Facet200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout, 'consistency' => consistency },
          body: facet_request
        )
      end

      def get(collection_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::GetCollection200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def list
        @connection.call(
          :GET,
          '/collections',
          type: Qdrant::Models::GetCollections200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def partial_update(collection_name:, timeout: nil, update_collection: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PATCH,
          '/collections/{collection_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: update_collection
        )
      end

      def update(collection_name:, timeout: nil, create_collection: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: create_collection
        )
      end
    end
  end
end
