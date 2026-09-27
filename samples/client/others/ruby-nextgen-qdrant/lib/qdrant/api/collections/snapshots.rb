# frozen_string_literal: true

module Qdrant
  module Api
    class Collections::Snapshots
      def initialize(connection)
        @connection = connection
      end

      def create(collection_name:, wait: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/snapshots'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateSnapshot200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait }
        )
      end

      def delete(collection_name:, snapshot_name:, wait: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'snapshot_name is required' if snapshot_name.nil?

        @connection.call(
          :DELETE,
          '/collections/{collection_name}/snapshots/{snapshot_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{snapshot_name}', ERB::Util.url_encode(snapshot_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait }
        )
      end

      def get(collection_name:, snapshot_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'snapshot_name is required' if snapshot_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/snapshots/{snapshot_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{snapshot_name}', ERB::Util.url_encode(snapshot_name.to_s)),
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def list(collection_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/snapshots'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::ListSnapshots200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def recover(collection_name:, wait: nil, snapshot_recover: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/snapshots/recover'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait },
          body: snapshot_recover
        )
      end

      def upload(collection_name:, wait: nil, priority: nil, checksum: nil, snapshot: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/snapshots/upload'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'priority' => priority, 'checksum' => checksum },
          form: { 'snapshot' => snapshot }
        )
      end
    end
  end
end
