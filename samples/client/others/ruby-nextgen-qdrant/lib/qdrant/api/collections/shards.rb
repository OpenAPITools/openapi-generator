# frozen_string_literal: true

module Qdrant
  module Api
    class Collections::Shards
      def initialize(connection)
        @connection = connection
      end

      def bulk_update(collection_name:, timeout: nil, create_sharding_key: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/shards'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: create_sharding_key
        )
      end

      def delete(collection_name:, timeout: nil, drop_sharding_key: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/shards/delete'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'timeout' => timeout },
          body: drop_sharding_key
        )
      end

      def snapshots(collection_name:, shard_id:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'shard_id is required' if shard_id.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/shards/{shard_id}/snapshots'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{shard_id}', ERB::Util.url_encode(shard_id.to_s)),
          type: Qdrant::Models::ListSnapshots200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def snapshots_delete(collection_name:, shard_id:, snapshot_name:, wait: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'shard_id is required' if shard_id.nil?
        raise ArgumentError, 'snapshot_name is required' if snapshot_name.nil?

        @connection.call(
          :DELETE,
          '/collections/{collection_name}/shards/{shard_id}/snapshots/{snapshot_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{shard_id}', ERB::Util.url_encode(shard_id.to_s))
            .gsub('{snapshot_name}', ERB::Util.url_encode(snapshot_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait }
        )
      end

      def snapshots_get(collection_name:, shard_id:, snapshot_name:)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'shard_id is required' if shard_id.nil?
        raise ArgumentError, 'snapshot_name is required' if snapshot_name.nil?

        @connection.call(
          :GET,
          '/collections/{collection_name}/shards/{shard_id}/snapshots/{snapshot_name}'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{shard_id}', ERB::Util.url_encode(shard_id.to_s))
            .gsub('{snapshot_name}', ERB::Util.url_encode(snapshot_name.to_s)),
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def snapshots_post(collection_name:, shard_id:, wait: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'shard_id is required' if shard_id.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/shards/{shard_id}/snapshots'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{shard_id}', ERB::Util.url_encode(shard_id.to_s)),
          type: Qdrant::Models::CreateSnapshot200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait }
        )
      end

      def snapshots_recover(collection_name:, shard_id:, wait: nil, shard_snapshot_recover: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'shard_id is required' if shard_id.nil?

        @connection.call(
          :PUT,
          '/collections/{collection_name}/shards/{shard_id}/snapshots/recover'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{shard_id}', ERB::Util.url_encode(shard_id.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait },
          body: shard_snapshot_recover
        )
      end

      def snapshots_upload(collection_name:, shard_id:, wait: nil, priority: nil, checksum: nil, snapshot: nil)
        raise ArgumentError, 'collection_name is required' if collection_name.nil?
        raise ArgumentError, 'shard_id is required' if shard_id.nil?

        @connection.call(
          :POST,
          '/collections/{collection_name}/shards/{shard_id}/snapshots/upload'
            .gsub('{collection_name}', ERB::Util.url_encode(collection_name.to_s))
            .gsub('{shard_id}', ERB::Util.url_encode(shard_id.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait, 'priority' => priority, 'checksum' => checksum },
          form: { 'snapshot' => snapshot }
        )
      end
    end
  end
end
