# frozen_string_literal: true

module Qdrant
  module Api
    class Collections::Index
      def initialize(connection)
        @connection = connection
      end

      def bulk_update(collection_name:, wait: nil, ordering: nil, create_field_index: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/index'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering },
          body: create_field_index
        )
      end

      def delete(collection_name:, field_name:, wait: nil, ordering: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'field_name is required' if field_name.nil?

        @connection.call(
          :DELETE,
          '/collections/{collection_name}/index/{field_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{field_name}', ERB::Util.url_encode(field_name.to_s)),
          type: Qdrant::Models::CreateFieldIndex200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'ordering' => ordering }
        )
      end
    end
  end
end
