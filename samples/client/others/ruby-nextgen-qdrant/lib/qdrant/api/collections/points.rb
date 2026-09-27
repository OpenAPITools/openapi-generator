# frozen_string_literal: true

module Qdrant
  module Api
    class Collections::Points
      def initialize(connection)
        @connection = connection
      end

      def batch(collection_name:, wait: nil, ordering: nil, update_operations: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/batch'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::BatchUpdate200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: update_operations
        )
      end

      def bulk_update(collection_name:, wait: nil, ordering: nil, point_insert_operations: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/points'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: point_insert_operations
        )
      end

      def count(collection_name:, timeout: nil, count_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/count'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CountPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: count_request
        )
      end

      def create(collection_name:, consistency: nil, timeout: nil, point_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::GetPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: point_request
        )
      end

      def delete(collection_name:, wait: nil, ordering: nil, points_selector: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/delete'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: points_selector
        )
      end

      def discover(collection_name:, consistency: nil, timeout: nil, discover_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/discover'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: discover_request
        )
      end

      def discover_batch(collection_name:, consistency: nil, timeout: nil, discover_request_batch: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/discover/batch'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchBatchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: discover_request_batch
        )
      end

      def get(collection_name:, id:, consistency: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'id is required' if id.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/points/{id}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{id}', ERB::Util.url_encode(id.to_s)),
          type: Qdrant::Models::GetPoint200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency }
        )
      end

      def payload(collection_name:, wait: nil, ordering: nil, set_payload: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/points/payload'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: set_payload
        )
      end

      def payload_clear(collection_name:, wait: nil, ordering: nil, points_selector: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/payload/clear'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: points_selector
        )
      end

      def payload_delete(collection_name:, wait: nil, ordering: nil, delete_payload: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/payload/delete'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: delete_payload
        )
      end

      def payload_post(collection_name:, wait: nil, ordering: nil, set_payload: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/payload'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: set_payload
        )
      end

      def query(collection_name:, consistency: nil, timeout: nil, query_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/query'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::QueryPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: query_request
        )
      end

      def query_batch(collection_name:, consistency: nil, timeout: nil, query_request_batch: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/query/batch'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::QueryBatchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: query_request_batch
        )
      end

      def query_groups(collection_name:, consistency: nil, timeout: nil, query_groups_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/query/groups'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchPointGroups200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: query_groups_request
        )
      end

      def recommend(collection_name:, consistency: nil, timeout: nil, recommend_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/recommend'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: recommend_request
        )
      end

      def recommend_batch(collection_name:, consistency: nil, timeout: nil, recommend_request_batch: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/recommend/batch'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchBatchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: recommend_request_batch
        )
      end

      def recommend_groups(collection_name:, consistency: nil, timeout: nil, recommend_groups_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/recommend/groups'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchPointGroups200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: recommend_groups_request
        )
      end

      def scroll(collection_name:, consistency: nil, timeout: nil, scroll_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/scroll'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::ScrollPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: scroll_request
        )
      end

      def search(collection_name:, consistency: nil, timeout: nil, search_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/search'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: search_request
        )
      end

      def search_batch(collection_name:, consistency: nil, timeout: nil, search_request_batch: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/search/batch'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchBatchPoints200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: search_request_batch
        )
      end

      def search_groups(collection_name:, consistency: nil, timeout: nil, search_groups_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/search/groups'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchPointGroups200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: search_groups_request
        )
      end

      def search_matrix_offsets(collection_name:, consistency: nil, timeout: nil, search_matrix_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/search/matrix/offsets'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchMatrixOffsets200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: search_matrix_request
        )
      end

      def search_matrix_pairs(collection_name:, consistency: nil, timeout: nil, search_matrix_request: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/search/matrix/pairs'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::SearchMatrixPairs200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'consistency' => consistency, 'timeout' => timeout },
          body: search_matrix_request
        )
      end

      def vectors(collection_name:, wait: nil, ordering: nil, update_vectors: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/points/vectors'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: update_vectors
        )
      end

      def vectors_delete(collection_name:, wait: nil, ordering: nil, delete_vectors: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/points/vectors/delete'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: delete_vectors
        )
      end
    end
  end
end
