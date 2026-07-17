# frozen_string_literal: true

module Qdrant
  module Api
    class Cluster::Peer
      def initialize(connection)
        @connection = connection
      end

      def delete(peer_id:, force: nil)
        raise ArgumentError, 'peer_id is required' if peer_id.nil?

        @connection.call(
          :DELETE,
          '/cluster/peer/{peer_id}'
            .gsub('{peer_id}', ERB::Util.url_encode(peer_id.to_s)),
          type: Qdrant::Models::CreateShardKey200Response,
          auth: ['api-key', 'bearerAuth'],
          query: { 'force' => force }
        )
      end
    end
  end
end
