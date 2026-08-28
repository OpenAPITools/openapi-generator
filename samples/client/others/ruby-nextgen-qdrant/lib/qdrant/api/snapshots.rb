# frozen_string_literal: true

module Qdrant
  module Api
    class Snapshots
      def initialize(connection)
        @connection = connection
      end

      def create(wait: nil)
        @connection.call(
          :POST,
          '/snapshots',
          type: Qdrant::Models::CreateSnapshot200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait }
        )
      end

      def delete(snapshot_name:, wait: nil)
        raise ArgumentError, 'snapshot_name is required' if snapshot_name.nil?

        @connection.call(
          :DELETE,
          '/snapshots/{snapshot_name}'
            .gsub('{snapshot_name}', ERB::Util.url_encode(snapshot_name.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'wait' => wait }
        )
      end

      def get(snapshot_name:)
        raise ArgumentError, 'snapshot_name is required' if snapshot_name.nil?

        @connection.call(
          :GET,
          '/snapshots/{snapshot_name}'
            .gsub('{snapshot_name}', ERB::Util.url_encode(snapshot_name.to_s)),
          type: nil,
          auth: ['api-key', 'bearerAuth']
        )
      end

      def list
        @connection.call(
          :GET,
          '/snapshots',
          type: Qdrant::Models::ListSnapshots200Response,
          auth: ['api-key', 'bearerAuth']
        )
      end
    end
  end
end
